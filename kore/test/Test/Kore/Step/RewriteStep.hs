module Test.Kore.Step.RewriteStep
    ( test_applyInitialConditions
    , test_renameRuleVariables
    , test_unifyRule
    , test_applyRewriteRule_
    , test_applyRewriteRulesParallel
    , test_applyRewriteRulesSequence
    , test_narrowing
    ) where

import Prelude.Kore

import Test.Tasty

import qualified Control.Exception as Exception
import Data.Default as Default
    ( def
    )
import qualified Data.Foldable as Foldable
import Data.Maybe
    ( fromJust
    )
import qualified Data.Set as Set

import Kore.Attribute.Pattern.FreeVariables
    ( FreeVariables
    )
import qualified Kore.Attribute.Pattern.FreeVariables as FreeVariables
import qualified Kore.Internal.Conditional as Conditional
import qualified Kore.Internal.MultiOr as MultiOr
import Kore.Internal.Predicate as Predicate
    ( makeAndPredicate
    , makeCeilPredicate
    , makeCeilPredicate_
    , makeEqualsPredicate
    , makeEqualsPredicate_
    , makeFalsePredicate_
    , makeNotPredicate
    , makeTruePredicate
    , makeTruePredicate_
    )
import qualified Kore.Internal.Predicate as Predicate
import qualified Kore.Internal.Substitution as Substitution
import Kore.Internal.TermLike
import Kore.Rewriting.RewritingVariable
import qualified Kore.Step.RewriteStep as Step
import Kore.Step.RulePattern
    ( RHS (..)
    , RewriteRule (..)
    , RulePattern (..)
    , injectTermIntoRHS
    , rulePattern
    )
import qualified Kore.Step.RulePattern as RulePattern
import qualified Kore.Step.Step as Step
import qualified Kore.Unification.Procedure as Unification
import Kore.Variables.Fresh
    ( nextName
    )
import qualified Logic

import Test.Kore.Internal.Condition as Condition
import Test.Kore.Internal.OrCondition
    ( OrTestCondition
    )
import Test.Kore.Internal.OrPattern
    ( OrTestPattern
    )
import qualified Test.Kore.Internal.OrPattern as OrPattern
import Test.Kore.Internal.Pattern as Pattern
import qualified Test.Kore.Step.MockSymbols as Mock
import Test.Kore.Step.Simplification
import Test.Tasty.HUnit.Ext

type RulePattern' = RulePattern VariableName
type Conditional' = Conditional VariableName
type RewriteRule' = RewriteRule VariableName
type Results' = Step.Results RulePattern VariableName

applyInitialConditions
    :: TestCondition
    -> TestCondition
    -> IO [OrTestCondition]
applyInitialConditions initial unification =
    Step.applyInitialConditions initial unification
    & runSimplifier Mock.env . Logic.observeAllT

test_applyInitialConditions :: [TestTree]
test_applyInitialConditions =
    [ testCase "\\bottom initial condition" $ do
        let unification =
                Conditional
                    { term = ()
                    , predicate = Predicate.makeTruePredicate_
                    , substitution = mempty
                    }
            initial = Condition.bottom
            expect = mempty
        actual <- applyInitialConditions initial unification
        assertEqual "" expect actual

    , testCase "returns axiom right-hand side" $ do
        let unification = Condition.top
            initial = Condition.top
            expect = [MultiOr.singleton initial]
        actual <- applyInitialConditions initial unification
        assertEqual "" expect actual

    , testCase "combine initial and rule conditions" $ do
        let unification = Condition.fromPredicate expect2
            initial = Condition.fromPredicate expect1
            expect1 =
                Predicate.makeEqualsPredicate_
                    (Mock.f $ mkElemVar Mock.x)
                    Mock.a
            expect2 =
                Predicate.makeEqualsPredicate_
                    (Mock.f $ mkElemVar Mock.y)
                    Mock.b
            expect =
                MultiOr.singleton (Predicate.makeAndPredicate expect1 expect2)
        [applied] <- applyInitialConditions initial unification
        let actual = Conditional.predicate <$> applied
        assertEqual "" expect actual

    , testCase "conflicting initial and rule conditions" $ do
        let predicate = Predicate.makeEqualsPredicate_ (mkElemVar Mock.x) Mock.a
            unification = Condition.fromPredicate predicate
            initial =
                Condition.fromPredicate
                $ Predicate.makeNotPredicate predicate
            expect = mempty
        actual <- applyInitialConditions initial unification
        assertEqual "" expect actual

    ]

unifyRule
    :: TestPattern
    -> RulePattern'
    -> IO [Conditional' RulePattern']
unifyRule initial rule =
    Step.unifyRule Unification.unificationProcedure initial rule
    & Logic.observeAllT
    & runSimplifier Mock.env

test_renameRuleVariables :: [TestTree]
test_renameRuleVariables =
    [ testCase "renames axiom left variables" $ do
        let initial =
                Step.mkRewritingPattern
                $ Pattern.fromTermLike
                $ Mock.f (mkElemVar Mock.x)
            axiom =
                RulePattern
                    { left = Mock.f (mkElemVar Mock.x)
                    , antiLeft = Nothing
                    , requires =
                        Predicate.makeEqualsPredicate_ (mkElemVar Mock.x) Mock.a
                    , rhs = injectTermIntoRHS (Mock.g (mkElemVar Mock.x))
                    , attributes = Default.def
                    }
            actual = mkRewritingRule axiom
            initialFreeVariables :: FreeVariables RewritingVariableName
            initialFreeVariables = freeVariables initial
            actualFreeVariables = freeVariables actual
        assertEqual "Expected no common free variables"
            Set.empty
            $ on Set.intersection FreeVariables.toSet
                initialFreeVariables
                actualFreeVariables

    ]

test_unifyRule :: [TestTree]
test_unifyRule =
    [ testCase "performs unification with initial term" $ do
        let initial = pure (Mock.functionalConstr10 Mock.a)
            axiom =
                RulePattern
                    { left = Mock.functionalConstr10 (mkElemVar Mock.x)
                    , antiLeft = Nothing
                    , requires = Predicate.makeTruePredicate_
                    , rhs = injectTermIntoRHS (Mock.g Mock.b)
                    , attributes = Default.def
                    }
            expect = [(pure axiom) { substitution }]
              where
                substitution =
                    Substitution.unsafeWrap [(inject Mock.x, Mock.a)]
        actual <- unifyRule initial axiom
        assertEqual "" expect actual

    , testCase "returns unification failures" $ do
        let initial = pure (Mock.functionalConstr10 Mock.a)
            axiom =
                RulePattern
                    { left = Mock.functionalConstr11 (mkElemVar Mock.x)
                    , antiLeft = Nothing
                    , requires = Predicate.makeTruePredicate_
                    , rhs = injectTermIntoRHS (Mock.g Mock.b)
                    , attributes = Default.def
                    }
            expect = []
        actual <- unifyRule initial axiom
        assertEqual "" expect actual
    ]

-- | Apply the 'RewriteRule' to the configuration, but discard remainders.
applyRewriteRule_
    ::  ( TestPattern -> [RewriteRule'] -> IO Results' )
    -- ^ 'RewriteRule'
    -> TestPattern
    -- ^ Configuration
    -> RewriteRule'
    -- ^ Rewrite rule
    -> IO [OrTestPattern]
applyRewriteRule_ applyRewriteRules initial rule =
    applyRewriteRules_ applyRewriteRules initial [rule]

-- | Apply the 'RewriteRule's to the configuration, but discard remainders.
applyRewriteRules_
    :: (TestPattern -> [RewriteRule'] -> IO Results')
    -- ^ 'RewriteRule's
    -> TestPattern
    -- ^ Configuration
    -> [RewriteRule']
    -- ^ Rewrite rule
    -> IO [OrTestPattern]
applyRewriteRules_ applyRewriteRules initial rules = do
    result <- applyRewriteRules initial rules
    return (Foldable.toList . discardRemainders $ result)
  where
    discardRemainders = fmap Step.result . Step.results

test_applyRewriteRule_ :: [TestTree]
test_applyRewriteRule_ =
    [ testCase "apply identity axiom" $ do
        let expect = [ OrPattern.fromPatterns [initial] ]
            initial = Pattern.fromTermLike (mkElemVar Mock.x)
        actual <- applyRewriteRuleParallel_ initial axiomId
        assertEqual "" expect actual

    , testCase "apply identity without renaming" $ do
        let expect =  [ OrPattern.fromPatterns [initial] ]
            initial = Pattern.fromTermLike (mkElemVar Mock.y)
        actual <- applyRewriteRuleParallel_ initial axiomId
        assertEqual "" expect actual

    , testCase "substitute variable with itself" $ do
        let expect =
                [ OrPattern.fromPatterns [initial { term = mkElemVar Mock.x }] ]
            initial = Pattern.fromTermLike
                (Mock.sigma (mkElemVar Mock.x) (mkElemVar Mock.x))
        actual <- applyRewriteRuleParallel_ initial axiomSigmaId
        assertEqual "" expect actual

    , testCase "merge configuration patterns" $ do
        let term = Mock.functionalConstr10 (mkElemVar Mock.y)
            expect =
                [ OrPattern.fromPatterns [initial { term, substitution }] ]
              where
                substitution =
                    Substitution.wrap
                    $ Substitution.mkUnwrappedSubstitution
                    [ (inject Mock.x, term) ]
            initial = Pattern.fromTermLike (Mock.sigma (mkElemVar Mock.x) term)
        actual <- applyRewriteRuleParallel_ initial axiomSigmaId
        assertEqual "" expect actual

    , testCase "substitution with symbol matching" $ do
        let expect =
                [ OrPattern.fromPatterns [initial { term = fz, substitution }] ]
              where
                substitution =
                    Substitution.wrap
                    $ Substitution.mkUnwrappedSubstitution
                    [ (inject Mock.y, mkElemVar Mock.z) ]
            fy = Mock.functionalConstr10 (mkElemVar Mock.y)
            fz = Mock.functionalConstr10 (mkElemVar Mock.z)
            initial = Pattern.fromTermLike (Mock.sigma fy fz)
        actual <- applyRewriteRuleParallel_ initial axiomSigmaId
        assertEqual "" expect actual

    , testCase "merge multiple variables" $ do
        let expect =
                [ OrPattern.fromPatterns [initial { term = yy, substitution }] ]
              where
                substitution =
                    Substitution.wrap
                    $ Substitution.mkUnwrappedSubstitution
                    [ (inject Mock.x, mkElemVar Mock.y) ]
            xy = Mock.sigma (mkElemVar Mock.x) (mkElemVar Mock.y)
            yx = Mock.sigma (mkElemVar Mock.y) (mkElemVar Mock.x)
            yy = Mock.sigma (mkElemVar Mock.y) (mkElemVar Mock.y)
            initial = Pattern.fromTermLike (Mock.sigma xy yx)
        actual <- applyRewriteRuleParallel_ initial axiomSigmaXXYY
        assertEqual "" expect actual

    , testCase "rename quantified right variables" $ do
        let expect =
                 [ OrPattern.fromPatterns [Pattern.fromTermLike final] ]
            final = mkElemVar Mock.y
            initial = pure (mkElemVar Mock.y)
            axiom =
                RewriteRule $ rulePattern
                    (mkElemVar Mock.x)
                    (mkExists Mock.y (mkElemVar Mock.x))
        actual <- applyRewriteRuleParallel_ initial axiom
        assertEqual "" expect actual

    , testCase "quantified rhs: non-clashing" $ do
        let expect =
                 [ OrPattern.fromPatterns [Pattern.fromTermLike final] ]
            x' =
                traverse (nextName (variableName Mock.x)) Mock.x
                & fromJust
            final = mkElemVar x'
            initial = pure (mkElemVar Mock.y)
            axiom =
                RewriteRule $ rulePattern
                    (mkElemVar Mock.x)
                    (mkExists Mock.x (mkElemVar Mock.x))
        actual <- applyRewriteRuleParallel_ initial axiom
        assertEqual "" expect actual

    , testCase "Apply non-function-like rule in parallel" $ do
        let
            initial = pure (Mock.sigma (mkElemVar Mock.x) (mkElemVar Mock.x))
        result <- Exception.try $ applyRewriteRuleParallel_
                                    initial
                                    axiomSigmaTopId
        case result of
            Left (Exception.ErrorCall _) -> return ()
            Right _ -> assertFailure "Expected error"

    , testCase "Apply list containing non-function-like rule in parallel" $ do
        let
            initial = pure (Mock.sigma (mkElemVar Mock.x) (mkElemVar Mock.x))
        result <- Exception.try $ applyRewriteRules_
                                    applyRewriteRulesParallel
                                    initial
                                    [axiomCaseA, axiomSigmaTopId]
        case result of
            Left (Exception.ErrorCall _) -> return ()
            Right _ -> assertFailure "Expected error"

    , testCase "Apply non-function-like rule in sequence" $ do
        let
            initial = pure (Mock.sigma (mkElemVar Mock.x) (mkElemVar Mock.x))
        result <- Exception.try $ applyRewriteRule_
                                    applyRewriteRulesSequence
                                    initial
                                    axiomSigmaTopId
        case result of
            Left (Exception.ErrorCall _) -> return ()
            Right _ -> assertFailure "Expected error"

    , testCase "Apply list containing non-function-like rule in sequence" $ do
        let
            initial = pure (Mock.sigma (mkElemVar Mock.x) (mkElemVar Mock.x))
        result <- Exception.try $ applyRewriteRules_
                                    applyRewriteRulesSequence
                                    initial
                                    [axiomCaseA, axiomSigmaTopId]
        case result of
            Left (Exception.ErrorCall _) -> return ()
            Right _ -> assertFailure "Expected error"

    , testCase "symbol clash" $ do
        let expect =  mempty
            fx = Mock.functionalConstr10 (mkElemVar Mock.x)
            gy = Mock.functionalConstr11 (mkElemVar Mock.y)
            initial = pure (Mock.sigma fx gy)
        actual <- applyRewriteRuleParallel_ initial axiomSigmaId
        assertEqual "" expect actual

    , testCase "impossible substitution" $ do
        let expect =  mempty
            xfy =
                Mock.sigma
                    (mkElemVar Mock.x)
                    (Mock.functionalConstr10 (mkElemVar Mock.y))
            xy = Mock.sigma (mkElemVar Mock.x) (mkElemVar Mock.y)
            initial = pure (Mock.sigma xfy xy)
        actual <- applyRewriteRuleParallel_ initial axiomSigmaXXYY
        assertEqual "" expect actual

    -- sigma(x, x) -> x
    -- vs
    -- sigma(a, f(b)) with substitution b=a
    , testCase "impossible substitution (ctor)" $ do
        let expect =  mempty
            initial =
                Conditional
                    { term =
                        Mock.sigma
                            (mkElemVar Mock.x)
                            (Mock.functionalConstr10 (mkElemVar Mock.y))
                    , predicate = Predicate.makeTruePredicate_
                    , substitution =
                        Substitution.wrap
                        $ Substitution.mkUnwrappedSubstitution
                        [(inject Mock.y, mkElemVar Mock.x)]
                    }
        actual <- applyRewriteRuleParallel_ initial axiomSigmaId
        assertEqual "" expect actual

    -- sigma(x, x) -> x
    -- vs
    -- sigma(a, h(b)) with substitution b=a
    , testCase "circular dependency error" $ do
        let expect =
                Conditional
                    { term = fy
                    , predicate =
                        makeEqualsPredicate Mock.testSort (mkElemVar Mock.y) fy
                    , substitution =
                        Substitution.wrap
                        $ Substitution.mkUnwrappedSubstitution
                        [(inject Mock.x, fy)]
                    }
                & pure . OrPattern.fromPattern
            fy = Mock.functional10 (mkElemVar Mock.y)
            initial =
                Conditional
                    { term = Mock.sigma (mkElemVar Mock.x) fy
                    , predicate = makeTruePredicate_
                    , substitution =
                        Substitution.wrap
                        $ Substitution.mkUnwrappedSubstitution
                        [(inject Mock.y, mkElemVar Mock.x)]
                    }
        actual <- applyRewriteRuleParallel_ initial axiomSigmaId
        assertEqual "" expect actual

    -- sigma(x, x) -> x
    -- vs
    -- sigma(sigma(a, a), sigma(sigma(b, c), sigma(b, b)))
    , testCase "unify all children" $ do
        let expect =

                    [ OrPattern.fromPatterns
                        [ Conditional
                            { term = Mock.sigma zz zz
                            , predicate = makeTruePredicate Mock.testSort
                            , substitution =
                                Substitution.wrap
                                $ Substitution.mkUnwrappedSubstitution
                                [ (inject Mock.x, zz)
                                , (inject Mock.y, mkElemVar Mock.z)
                                ]
                            }
                        ]
                    ]
            xx = Mock.sigma (mkElemVar Mock.x) (mkElemVar Mock.x)
            yy = Mock.sigma (mkElemVar Mock.y) (mkElemVar Mock.y)
            zz = Mock.sigma (mkElemVar Mock.z) (mkElemVar Mock.z)
            yz = Mock.sigma (mkElemVar Mock.y) (mkElemVar Mock.z)
            initial = pure $ Mock.sigma xx (Mock.sigma yz yy)
        actual <- applyRewriteRuleParallel_ initial axiomSigmaId
        assertEqual "" expect actual

    -- sigma(sigma(x, x), y) => sigma(x, y)
    -- vs
    -- sigma(sigma(a, f(b)), a)
    -- Expected: sigma(f(b), f(b)) and a=f(b)
    , testCase "normalize substitution" $ do
        let
            fb = Mock.functional10 (mkElemVar Mock.y)
            expect =

                    [ OrPattern.fromPatterns
                        [ Conditional
                            { term = Mock.sigma fb fb
                            , predicate = makeTruePredicate Mock.testSort
                            , substitution =
                                Substitution.wrap
                                $ Substitution.mkUnwrappedSubstitution
                                [(inject Mock.x, fb)]
                            }
                        ]
                    ]
            initial = pure $
                Mock.sigma(Mock.sigma (mkElemVar Mock.x) fb) (mkElemVar Mock.x)
        actual <- applyRewriteRuleParallel_ initial axiomSigmaXXY
        assertEqual "" expect actual

    -- sigma(sigma(x, x), y) => sigma(x, y)
    -- vs
    -- sigma(sigma(a, f(b)), a) and a=f(c)
    -- Expected: sigma(f(b), f(b)) and a=f(b), b=c
    , testCase "merge substitution with initial" $ do
        let
            fy = Mock.functionalConstr10 (mkElemVar Mock.y)
            fz = Mock.functionalConstr10 (mkElemVar Mock.z)
            expect =

                    [ OrPattern.fromPatterns
                        [ Conditional
                            { term = Mock.sigma fz fz
                            , predicate = makeTruePredicate Mock.testSort
                            , substitution =
                                Substitution.wrap
                                $ Substitution.mkUnwrappedSubstitution
                                    [ (inject Mock.x, fz)
                                    , (inject Mock.y, mkElemVar Mock.z)
                                    ]
                            }
                        ]
                    ]
            initial =
                Conditional
                    { term =
                        Mock.sigma
                            (Mock.sigma (mkElemVar Mock.x) fy)
                            (mkElemVar Mock.x)
                    , predicate = makeTruePredicate_
                    , substitution =
                        Substitution.wrap
                        $ Substitution.mkUnwrappedSubstitution
                        [(inject Mock.x, fz)]
                    }
        actual <- applyRewriteRuleParallel_ initial axiomSigmaXXY
        assertEqual "" expect actual

    -- "sl1" => x
    -- vs
    -- "sl2"
    -- Expected: bottom
    , testCase "unmatched string literals" $ do
        let expect =  mempty
            initial = pure (mkStringLiteral "sl2")
            axiom =
                RewriteRule $ rulePattern
                    (mkStringLiteral "sl1")
                    (mkElemVar Mock.x)
        actual <- applyRewriteRuleParallel_ initial axiom
        assertEqual "" expect actual

    -- x => x
    -- vs
    -- a and g(a)=f(a)
    -- Expected: a and g(a)=f(a)
    , testCase "preserve initial condition" $ do
        let expect =  [ OrPattern.fromPatterns [initial] ]
            predicate =
                makeEqualsPredicate Mock.testSort
                    (Mock.functional11 Mock.a)
                    (Mock.functional10 Mock.a)
            initial =
                Conditional
                    { term = Mock.a
                    , predicate
                    , substitution = mempty
                    }
        actual <- applyRewriteRuleParallel_ initial axiomId
        assertEqual "" expect actual

    -- sigma(sigma(x, x), y) => sigma(x, y)
    -- vs
    -- sigma(sigma(a, f(b)), a) and g(a)=f(a)
    -- Expected: sigma(f(b), f(b)) and a=f(b) and and g(f(b))=f(f(b))
    , testCase "normalize substitution with initial condition" $ do
        let
            fb = Mock.functional10 (mkElemVar Mock.y)
            expect =

                    [ OrPattern.fromPatterns
                        [ Conditional
                            { term = Mock.sigma fb fb
                            , predicate =
                                makeEqualsPredicate Mock.testSort
                                    (Mock.functional11 fb)
                                    (Mock.functional10 fb)
                            , substitution =
                                Substitution.wrap
                                $ Substitution.mkUnwrappedSubstitution
                                [(inject Mock.x, fb)]
                            }
                        ]
                    ]
            initial =
                Conditional
                    { term =
                        Mock.sigma
                            (Mock.sigma (mkElemVar Mock.x) fb)
                            (mkElemVar Mock.x)
                    , predicate =
                        makeEqualsPredicate_
                            (Mock.functional11 (mkElemVar Mock.x))
                            (Mock.functional10 (mkElemVar Mock.x))
                    , substitution = mempty
                    }
        actual <- applyRewriteRuleParallel_ initial axiomSigmaXXY
        assertEqual "" expect actual

    -- x => x ensures g(x)=f(x)
    -- vs
    -- y
    -- Expected: y and g(y)=f(y)
    , testCase "conjoin rule ensures" $ do
        let
            ensures =
                makeEqualsPredicate_
                    (Mock.functional11 (mkElemVar Mock.x))
                    (Mock.functional10 (mkElemVar Mock.x))
            rhs = (RulePattern.rhs ruleId) { ensures }
            expect :: [OrTestPattern]
            expect =
                [ OrPattern.fromPatterns
                    [ Conditional
                        { term = mkElemVar Mock.y
                        , predicate = makeEqualsPredicate Mock.testSort
                            (Mock.functional11 (mkElemVar Mock.y))
                            (Mock.functional10 (mkElemVar Mock.y))
                        , substitution = mempty
                        }
                    ]
                ]
            initial = Pattern.fromTermLike (mkElemVar Mock.y)
            axiom = RewriteRule ruleId { rhs }
        actual <- applyRewriteRuleParallel_ initial axiom
        assertEqual "" expect actual

    -- x => x requires g(x)=f(x)
    -- vs
    -- a
    -- Expected: y1 and g(a)=f(a)
    , testCase "conjoin rule requirement" $ do
        let
            requires =
                makeEqualsPredicate_
                    (Mock.functional11 (mkElemVar Mock.x))
                    (Mock.functional10 (mkElemVar Mock.x))
            expect =
                [ OrPattern.fromPatterns
                    [ initialTerm
                    `Pattern.withCondition` Condition.fromPredicate requires
                    ]
                ]
            initialTerm = mkElemVar Mock.x
            initial = pure initialTerm
            axiom = RewriteRule ruleId { requires }
        actual <- applyRewriteRuleParallel_ initial axiom
        assertEqual "" expect actual

    , testCase "rule a => \\bottom" $ do
        let expect =  [ OrPattern.fromPatterns [] ]
            initial = pure Mock.a
        actual <- applyRewriteRuleParallel_ initial axiomBottom
        assertEqual "" expect actual

    , testCase "rule a => b ensures \\bottom" $ do
        let expect =  [ OrPattern.fromPatterns [] ]
            initial = pure Mock.a
        actual <- applyRewriteRuleParallel_ initial axiomEnsuresBottom
        assertEqual "" expect actual

    , testCase "rule a => b requires \\bottom" $ do
        let expect =  [ ]
            initial = pure Mock.a
        actual <- applyRewriteRuleParallel_ initial axiomRequiresBottom
        assertEqual "" expect actual

    , testCase "rule a => \\bottom does not apply to c" $ do
        let expect =  [ ]
            initial = pure Mock.c
        actual <- applyRewriteRuleParallel_
                    initial
                    axiomRequiresBottom
        assertEqual "" expect actual
    ]
  where
    applyRewriteRuleParallel_ = applyRewriteRule_ applyRewriteRulesParallel

    ruleId =
        rulePattern
            (mkElemVar Mock.x)
            (mkElemVar Mock.x)
    axiomId = RewriteRule ruleId

    axiomBottom =
        RewriteRule RulePattern
            { left = Mock.a
            , antiLeft = Nothing
            , requires = makeTruePredicate_
            , rhs = injectTermIntoRHS (mkBottom Mock.testSort)
            , attributes = def
            }

    axiomEnsuresBottom =
        RewriteRule RulePattern
            { left = Mock.a
            , antiLeft = Nothing
            , requires = makeTruePredicate_
            , rhs = RHS
                { existentials = []
                , right = Mock.b
                , ensures = makeFalsePredicate_
                }
            , attributes = def
            }

    axiomRequiresBottom =
        RewriteRule RulePattern
            { left = Mock.a
            , antiLeft = Nothing
            , requires = makeFalsePredicate_
            , rhs = injectTermIntoRHS Mock.b
            , attributes = def
            }

    axiomSigmaId =
        RewriteRule $ rulePattern
            (Mock.sigma (mkElemVar Mock.x) (mkElemVar Mock.x))
            (mkElemVar Mock.x)

    axiomSigmaTopId =
        RewriteRule $ rulePattern
            (Mock.sigma (mkElemVar Mock.x) mkTop_)
            (mkElemVar Mock.x)

    axiomSigmaXXYY =
        RewriteRule $ rulePattern
            (Mock.sigma
                    (Mock.sigma (mkElemVar Mock.x) (mkElemVar Mock.x))
                    (Mock.sigma (mkElemVar Mock.y) (mkElemVar Mock.y))
            )
            (Mock.sigma (mkElemVar Mock.x) (mkElemVar Mock.y))

    axiomSigmaXXY =
        RewriteRule $ rulePattern
            (Mock.sigma
                    (Mock.sigma (mkElemVar Mock.x) (mkElemVar Mock.x))
                    (mkElemVar Mock.y)
            )
            (Mock.sigma (mkElemVar Mock.x) (mkElemVar Mock.y))

{- | Tests for symbolic narrowing.

Narrowing happens when a variable in a symbolic configuration is instantiated
with a particular value.

 -}
test_narrowing :: [TestTree]
test_narrowing =
    [ testCase "applyRewriteRulesParallel" $ do
        actual <- apply axiom (Pattern.fromTermLike initial)
        let results = OrPattern.fromPatterns [result]
        checkResults results actual
        let remainders = OrPattern.fromPatterns [remainder]
        checkRemainders remainders actual
    , testCase "getResultPattern" $ do
        let resultRewriting =
                Pattern.withCondition (Mock.sigma Mock.b (mkElemVar xRule))
                $ Condition.fromSingleSubstitution
                $ Substitution.assign
                    (inject xConfig)
                    (Mock.sigma Mock.a (mkElemVar xRule))
            initialVariables = FreeVariables.freeVariable (inject xConfig)
            actual = getResultPattern initialVariables resultRewriting
        assertEqual "" result actual
    ]
  where
    apply rule config = applyRewriteRulesParallel config [rule]
    x = Mock.x
    x' = traverse (\name -> nextName name name) x & fromJust
    xConfig = mkElementConfigVariable x
    xRule = mkElementRuleVariable x
    initial = mkElemVar x
    -- The significance of this axiom is that it narrows the initial term and
    -- introduces a new variable.
    axiom =
        RewriteRule $ rulePattern
            (Mock.sigma Mock.a (mkElemVar x))
            (Mock.sigma Mock.b (mkElemVar x))
    result =
        Pattern.withCondition (Mock.sigma Mock.b (mkElemVar x'))
        $ Condition.fromSingleSubstitution
        $ Substitution.assign (inject x) (Mock.sigma Mock.a (mkElemVar x'))
    remainder =
        Pattern.withCondition initial
        $ Condition.fromPredicate
        $ Predicate.makeNotPredicate
        $ Predicate.makeExistsPredicate x'
        $ Predicate.makeEqualsPredicate_
            (mkElemVar x)
            (Mock.sigma Mock.a (mkElemVar x'))

-- | Apply the 'RewriteRule's to the configuration.
applyRewriteRulesParallel
    :: TestPattern
    -- ^ Configuration
    -> [RewriteRule']
    -- ^ Rewrite rule
    -> IO Results'
applyRewriteRulesParallel initial rules =
    Step.applyRewriteRulesParallel
        Unification.unificationProcedure
        (mkRewritingRule <$> rules)
        (simplifiedPattern initial)
    & runSimplifierNoSMT Mock.env

checkResults
    :: HasCallStack
    => OrTestPattern
    -> Results'
    -> Assertion
checkResults expect actual =
    assertEqual "compare results"
        expect
        (Step.gatherResults actual)

checkRemainders
    :: HasCallStack
    => OrTestPattern
    -> Results'
    -> Assertion
checkRemainders expect actual =
    assertEqual "compare remainders"
        expect
        (Step.remainders actual)

test_applyRewriteRulesParallel :: [TestTree]
test_applyRewriteRulesParallel =
    [ testCase "if _ then _" $ do
        -- This uses `functionalConstr20(x, y)` instead of `if x then y`
        -- and `a` instead of `true`.
        --
        -- Intended:
        --   term: if x then cg
        --   axiom: if true y => y
        -- Actual:
        --   term: constr20(x, cg)
        --   axiom: constr20(a, y) => y
        -- Expected:
        --   rewritten: cg, with ⌈cg⌉ and [x=a]
        --   remainder: constr20(x, cg), with ¬(⌈cg⌉ and x=a)
        let
            results =
                MultiOr.singleton Conditional
                    { term = Mock.cg
                    , predicate = makeCeilPredicate Mock.testSort Mock.cg
                    , substitution =
                        Substitution.wrap
                        $ Substitution.mkUnwrappedSubstitution
                        [(inject Mock.x, Mock.a)]
                    }
            remainders =
                OrPattern.fromPatterns
                    [ Conditional
                        { term = initialTerm
                        , predicate =
                            makeNotPredicate
                            $ makeAndPredicate
                                (makeCeilPredicate Mock.testSort Mock.cg)
                                (makeEqualsPredicate Mock.testSort
                                    (mkElemVar Mock.x)
                                    Mock.a
                                )
                        , substitution = mempty
                        }
                    ]
            initialTerm = Mock.functionalConstr20 (mkElemVar Mock.x) Mock.cg
            initial = pure initialTerm
        actual <- applyRewriteRulesParallel initial [axiomIfThen]
        checkResults results actual
        checkRemainders remainders actual

    , testCase "if _ then _ with initial condition" $ do
        -- This uses `functionalConstr20(x, y)` instead of `if x then y`
        -- and `a` instead of `true`.
        --
        -- Intended:
        --   term: if x then cg
        --   axiom: if true y => y
        -- Actual:
        --   term: constr20(x, cg), with a ⌈cf⌉ predicate
        --   axiom: constr20(a, y) => y
        -- Expected:
        --   rewritten: cg, with ⌈cf⌉ and ⌈cg⌉ and [x=a]
        --   remainder: constr20(x, cg), with ⌈cf⌉ and ¬(⌈cg⌉ and x=a)
        let
            results =
                MultiOr.singleton Conditional
                    { term = Mock.cg
                    , predicate =
                        makeAndPredicate
                            (makeCeilPredicate Mock.testSort Mock.cf)
                            (makeCeilPredicate_ Mock.cg)
                    , substitution =
                        Substitution.wrap
                        $ Substitution.mkUnwrappedSubstitution
                            [(inject Mock.x, Mock.a)]
                    }
            remainders =
                OrPattern.fromPatterns
                    [ Conditional
                        { term =
                            Mock.functionalConstr20
                                (mkElemVar Mock.x)
                                Mock.cg
                        , predicate =
                            makeAndPredicate
                                (makeCeilPredicate Mock.testSort Mock.cf)
                                (makeNotPredicate $ makeAndPredicate
                                    (makeCeilPredicate Mock.testSort Mock.cg)
                                    (makeEqualsPredicate Mock.testSort
                                        (mkElemVar Mock.x)
                                        Mock.a
                                    )
                                )
                        , substitution = mempty
                        }
                    ]
            initialTerm = Mock.functionalConstr20 (mkElemVar Mock.x) Mock.cg
            initial =
                Conditional
                    { term = initialTerm
                    , predicate = makeCeilPredicate_ Mock.cf
                    , substitution = mempty
                    }
        actual <- applyRewriteRulesParallel initial [axiomIfThen]
        checkResults results actual
        checkRemainders remainders actual

    , testCase "signum - side condition" $ do
        -- This uses `functionalConstr20(x, y)` instead of `if x then y`
        -- and `a` instead of `true`.
        --
        -- Intended:
        --   term: signum(x)
        --   axiom: signum(y) => -1 if (y<0 == true)
        -- Actual:
        --   term: functionalConstr10(x)
        --   axiom: functionalConstr10(y) => a if f(y) == b
        -- Expected:
        --   rewritten: a, with f(x) == b
        --   remainder: functionalConstr10(x), with ¬(f(x) == b)
        let
            results =
                MultiOr.singleton Conditional
                    { term = Mock.a
                    , predicate = requirement
                    , substitution = mempty
                    }
            remainders =
                MultiOr.singleton Conditional
                    { term = Mock.functionalConstr10 (mkElemVar Mock.x)
                    , predicate = makeNotPredicate requirement
                    , substitution = mempty
                    }
            initial = pure (Mock.functionalConstr10 (mkElemVar Mock.x))
            requirement =
                makeEqualsPredicate Mock.testSort
                    (Mock.f (mkElemVar Mock.x))
                    Mock.b
        actual <- applyRewriteRulesParallel initial [axiomSignum]
        checkResults results actual
        checkRemainders remainders actual

    , testCase "if _ then _ -- partial" $ do
        -- This uses `functionalConstr20(x, y)` instead of `if x then y`
        -- and `a` instead of `true`.
        --
        -- Intended:
        --   term: if x then cg
        --   axiom: if true y => y
        -- Actual:
        --   term: functionalConstr20(x, cg)
        --   axiom: functionalConstr20(a, y) => y
        -- Expected:
        --   rewritten: cg, with ⌈cg⌉ and [x=a]
        --   remainder: functionalConstr20(x, cg), with ¬(⌈cg⌉ and x=a)
        let
            results =
                MultiOr.singleton Conditional
                    { term = Mock.cg
                    , predicate = makeCeilPredicate Mock.testSort Mock.cg
                    , substitution =
                        Substitution.wrap
                        $ Substitution.mkUnwrappedSubstitution
                        [(inject Mock.x, Mock.a)]
                    }
            remainders =
                OrPattern.fromPatterns
                    [ initial
                        { predicate =
                            makeNotPredicate
                            $ makeAndPredicate
                                (makeCeilPredicate Mock.testSort Mock.cg)
                                (makeEqualsPredicate Mock.testSort
                                    (mkElemVar Mock.x)
                                    Mock.a
                                )
                        }
                    ]
            initialTerm = Mock.functionalConstr20 (mkElemVar Mock.x) Mock.cg
            initial = pure initialTerm
        actual <- applyRewriteRulesParallel initial [axiomIfThen]
        checkResults results actual
        checkRemainders remainders actual

    , testCase "case _ of a -> _; b -> _ -- partial" $ do
        -- This uses `functionalConstr30(x, y, z)` to represent a case
        -- statement,
        -- i.e. `case x of 1 -> y; 2 -> z`
        -- and `a`, `b` as the case labels.
        --
        -- Intended:
        --   term: case x of 1 -> cf; 2 -> cg
        --   axiom: case 1 of 1 -> cf; 2 -> cg => cf
        --   axiom: case 2 of 1 -> cf; 2 -> cg => cg
        -- Actual:
        --   term: constr30(x, cg, cf)
        --   axiom: constr30(a, y, z) => y
        --   axiom: constr30(b, y, z) => z
        -- Expected:
        --   rewritten: cf, with (⌈cf⌉ and ⌈cg⌉) and [x=a]
        --   rewritten: cg, with (⌈cf⌉ and ⌈cg⌉) and [x=b]
        --   remainder:
        --     constr20(x, cf, cg)
        --        with ¬(⌈cf⌉ and ⌈cg⌉ and [x=a])
        --         and ¬(⌈cf⌉ and ⌈cg⌉ and [x=b])
        let
            definedBranches =
                makeAndPredicate
                    (makeCeilPredicate Mock.testSort Mock.cf)
                    (makeCeilPredicate_ Mock.cg)
            results =
                OrPattern.fromPatterns
                    [ Conditional
                        { term = Mock.cf
                        , predicate = definedBranches
                        , substitution =
                            Substitution.wrap
                            $ Substitution.mkUnwrappedSubstitution
                            [(inject Mock.x, Mock.a)]
                        }
                    , Conditional
                        { term = Mock.cg
                        , predicate = definedBranches
                        , substitution =
                            Substitution.wrap
                            $ Substitution.mkUnwrappedSubstitution
                            [(inject Mock.x, Mock.b)]
                        }
                    ]
            remainders =
                OrPattern.fromPatterns
                    [ initial
                        { predicate =
                            Predicate.makeAndPredicate
                                (makeNotPredicate $ makeAndPredicate
                                    definedBranches
                                    (Predicate.makeEqualsPredicate Mock.testSort
                                        (mkElemVar Mock.x)
                                        Mock.a
                                    )
                                )
                                (makeNotPredicate $ makeAndPredicate
                                    definedBranches
                                    (Predicate.makeEqualsPredicate Mock.testSort
                                        (mkElemVar Mock.x)
                                        Mock.b
                                    )
                                )
                        }
                    ]
            initialTerm =
                Mock.functionalConstr30 (mkElemVar Mock.x) Mock.cf Mock.cg
            initial = pure initialTerm
        actual <- applyRewriteRulesParallel initial axiomsCase
        checkResults results actual
        checkRemainders remainders actual

    , testCase "if _ then _ -- partial" $ do
        -- This uses `functionalConstr20(x, y)` instead of `if x then y`
        -- and `a` instead of `true`.
        --
        -- Intended:
        --   term: if x then cg
        --   axiom: if true y => y
        -- Actual:
        --   term: functionalConstr20(x, cg)
        --   axiom: functionalConstr20(a, y) => y
        -- Expected:
        --   rewritten: cg, with ⌈cg⌉ and [x=a]
        --   remainder: functionalConstr20(x, cg), with ¬(⌈cg⌉ and x=a)
        let
            results =
                MultiOr.singleton Conditional
                    { term = Mock.cg
                    , predicate = makeCeilPredicate Mock.testSort Mock.cg
                    , substitution =
                        Substitution.wrap
                        $ Substitution.mkUnwrappedSubstitution
                        [(inject Mock.x, Mock.a)]
                    }
            remainders =
                OrPattern.fromPatterns
                    [ initial
                        { predicate =
                            makeNotPredicate $ makeAndPredicate
                                (makeCeilPredicate Mock.testSort Mock.cg)
                                (makeEqualsPredicate Mock.testSort
                                    (mkElemVar Mock.x)
                                    Mock.a
                                )
                        }
                    ]
            initialTerm = Mock.functionalConstr20 (mkElemVar Mock.x) Mock.cg
            initial = pure initialTerm
        actual <- applyRewriteRulesParallel initial [axiomIfThen]
        checkResults results actual
        checkRemainders remainders actual

    , testCase "adding variables" $ do
        -- Term: a
        -- Rule: a => x
        -- Expected: exists x . x
        let
            results =
                OrPattern.fromTermLike (mkElemVar Mock.x)
            remainders = OrPattern.bottom
            initialTerm = Mock.a
            initial = Pattern.fromTermLike initialTerm
        actual <- applyRewriteRulesParallel initial
            [ RewriteRule RulePattern
                { left = Mock.a
                , antiLeft = Nothing
                , requires = makeTruePredicate_
                , rhs = injectTermIntoRHS (mkElemVar Mock.x)
                , attributes = def
                }
            ]
        checkResults results actual
        checkRemainders remainders actual
    ]

axiomIfThen :: RewriteRule'
axiomIfThen =
    RewriteRule $ rulePattern
        (Mock.functionalConstr20 Mock.a (mkElemVar Mock.y))
        (mkElemVar Mock.y)

axiomSignum :: RewriteRule'
axiomSignum =
    RewriteRule RulePattern
        { left = Mock.functionalConstr10 (mkElemVar Mock.y)
        , antiLeft = Nothing
        , requires = makeEqualsPredicate_ (Mock.f (mkElemVar Mock.y)) Mock.b
        , rhs = injectTermIntoRHS Mock.a
        , attributes = def
        }

axiomCaseA :: RewriteRule'
axiomCaseA =
    RewriteRule $ rulePattern
        (Mock.functionalConstr30
                Mock.a
                (mkElemVar Mock.y)
                (mkElemVar Mock.z)
        )
        (mkElemVar Mock.y)

axiomCaseB :: RewriteRule'
axiomCaseB =
    RewriteRule $ rulePattern
        (Mock.functionalConstr30
                Mock.b
                (mkElemVar Mock.y)
                (mkElemVar Mock.z)
        )
        (mkElemVar Mock.z)

axiomsCase :: [RewriteRule']
axiomsCase = [axiomCaseA, axiomCaseB]


-- | Apply the 'RewriteRule's to the configuration in sequence.
applyRewriteRulesSequence
    :: TestPattern
    -- ^ Configuration
    -> [RewriteRule']
    -- ^ Rewrite rule
    -> IO Results'
applyRewriteRulesSequence initial rules =
    Step.applyRewriteRulesSequence
        Unification.unificationProcedure
        (simplifiedPattern initial)
        (mkRewritingRule <$> rules)
    & runSimplifier Mock.env

test_applyRewriteRulesSequence :: [TestTree]
test_applyRewriteRulesSequence =
    [ testCase "case _ of a -> _; b -> _ -- partial" $ do
        -- This uses `functionalConstr30(x, y, z)` to represent a case
        -- statement,
        -- i.e. `case x of 1 -> y; 2 -> z`
        -- and `a`, `b` as the case labels.
        --
        -- Intended:
        --   term: case x of 1 -> cf; 2 -> cg
        --   axiom: case 1 of 1 -> cf; 2 -> cg => cf
        --   axiom: case 2 of 1 -> cf; 2 -> cg => cg
        -- Actual:
        --   term: constr30(x, cg, cf)
        --   axiom: constr30(a, y, z) => y
        --   axiom: constr30(b, y, z) => z
        -- Expected:
        --   rewritten: cf, with (⌈cf⌉ and ⌈cg⌉) and [x=a]
        --   rewritten: cg, with (⌈cf⌉ and ⌈cg⌉) and [x=b]
        --   remainder:
        --     constr20(x, cf, cg)
        --        with ¬(⌈cf⌉ and [x=a])
        --         and ¬(⌈cg⌉ and [x=b])
        let
            definedBranches =
                makeAndPredicate
                    (makeCeilPredicate Mock.testSort Mock.cf)
                    (makeCeilPredicate_ Mock.cg)
            results =
                OrPattern.fromPatterns
                    [ Conditional
                        { term = Mock.cf
                        , predicate = definedBranches
                        , substitution =
                            Substitution.wrap
                            $ Substitution.mkUnwrappedSubstitution
                            [(inject Mock.x, Mock.a)]
                        }
                    , Conditional
                        { term = Mock.cg
                        , predicate = definedBranches
                        , substitution =
                            Substitution.wrap
                            $ Substitution.mkUnwrappedSubstitution
                            [(inject Mock.x, Mock.b)]
                        }
                    ]
            remainders =
                OrPattern.fromPatterns
                    [ initial
                        { predicate =
                            Predicate.makeAndPredicate
                                (Predicate.makeNotPredicate
                                    $ Predicate.makeAndPredicate
                                        definedBranches
                                        (Predicate.makeEqualsPredicate_
                                            (mkElemVar Mock.x)
                                            Mock.a
                                        )
                                )
                                (Predicate.makeNotPredicate
                                    $ Predicate.makeAndPredicate
                                        definedBranches
                                        (Predicate.makeEqualsPredicate_
                                            (mkElemVar Mock.x)
                                            Mock.b
                                        )
                                )
                        }
                    ]
            initialTerm =
                Mock.functionalConstr30 (mkElemVar Mock.x) Mock.cf Mock.cg
            initial = pure initialTerm
        actual <- applyRewriteRulesSequence initial axiomsCase
        checkResults results actual
        checkRemainders remainders actual

    , testCase "adding variables" $ do
        -- Term: a
        -- Rule: a => x
        -- Expected: exists x . x
        let
            results =
                OrPattern.fromTermLike (mkElemVar Mock.x)
            remainders = OrPattern.bottom
            initialTerm = Mock.a
            initial = Pattern.fromTermLike initialTerm
        actual <- applyRewriteRulesSequence initial
            [ RewriteRule $ rulePattern
                Mock.a
                (mkElemVar Mock.x)
            ]
        checkResults results actual
        checkRemainders remainders actual
    ]
