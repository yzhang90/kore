module Test.Kore.Step.Rule.Combine
    ( test_combineRulesPredicate
    , test_combineRules
    , test_combineRulesGrouped
    ) where

import Prelude.Kore

import Test.Tasty

import Data.Default
    ( def
    )
import Data.Text
    ( Text
    )

import Kore.Internal.ApplicationSorts
    ( applicationSorts
    )
import Kore.Internal.Predicate
    ( Predicate
    , makeAndPredicate
    , makeCeilPredicate_
    , makeEqualsPredicate_
    , makeMultipleAndPredicate
    , makeNotPredicate
    , makeTruePredicate_
    )
import Kore.Internal.TermLike
    ( Alias (Alias)
    , TermLike
    , mkAnd
    , mkApplyAlias
    , mkBottom_
    , mkElemVar
    , mkEquals_
    , mkOr
    )
import qualified Kore.Internal.TermLike as TermLike.DoNotUse
import Kore.Step.AntiLeft
    ( AntiLeft
    )
import qualified Kore.Step.AntiLeft as AntiLeft
    ( parse
    )
import Kore.Step.Rule.Combine
import Kore.Step.RulePattern
    ( RHS (..)
    , RewriteRule (RewriteRule)
    , RulePattern (RulePattern)
    )
import qualified Kore.Step.RulePattern as RulePattern
    ( RulePattern (..)
    )
import Kore.Step.Simplification.Data
    ( runSimplifier
    )
import Kore.Syntax.Variable
import Kore.Unparser
    ( unparseToString
    )

import Test.Kore
    ( testId
    )
import qualified Test.Kore.Step.MockSymbols as Mock
import Test.SMT
    ( runNoSMT
    )
import Test.Tasty.HUnit.Ext

class RewriteRuleBase base where
    rewritesTo
        :: base VariableName -> base VariableName -> RewriteRule VariableName

newtype Pair variable = Pair (TermLike variable, Predicate variable)

instance RewriteRuleBase Pair where
    Pair (t1, p1) `rewritesTo` Pair (t2, p2) =
        RewriteRule RulePattern
            { left = t1
            , requires = p1
            , rhs = RHS
                { existentials = []
                , right = t2
                , ensures = p2
                }
            , antiLeft = Nothing
            , attributes = def
            }

instance RewriteRuleBase TermLike where
    t1 `rewritesTo` t2 =
        Pair (t1, makeTruePredicate_) `rewritesTo` Pair (t2, makeTruePredicate_)

withAntiLeft
    :: RewriteRule VariableName
    -> AntiLeft VariableName
    -> RewriteRule VariableName
withAntiLeft (RewriteRule rule) antiLeft =
    RewriteRule rule {RulePattern.antiLeft = Just antiLeft}

parseAntiLeft
    :: TermLike VariableName
    -> IO (AntiLeft VariableName)
parseAntiLeft term =
    case AntiLeft.parse term of
        Nothing -> assertFailure
            ("Cannot interpret " ++ unparseToString term ++ " as antileft.")
        Just antiLeft -> return antiLeft

test_combineRulesPredicate :: [TestTree]
test_combineRulesPredicate =
    [ testCase "One rule" $
        let expected = makeTruePredicate_
            actual = mergeRulesPredicate
                [ Mock.a `rewritesTo` Mock.cf ]
        in assertEqual "" expected actual
    , testCase "Two rules" $
        let expected = makeCeilPredicate_ (mkAnd Mock.cf Mock.b)
            actual = mergeRulesPredicate
                [ Mock.a `rewritesTo` Mock.cf
                , Mock.b `rewritesTo` Mock.cg
                ]
        in assertEqual "" expected actual
    , testCase "Three rules case" $
        let expected =
                makeAndPredicate
                    (makeCeilPredicate_ (mkAnd Mock.cf Mock.b))
                    (makeCeilPredicate_ (mkAnd Mock.cg Mock.c))

            actual = mergeRulesPredicate
                [ Mock.a `rewritesTo` Mock.cf
                , Mock.b `rewritesTo` Mock.cg
                , Mock.c `rewritesTo` Mock.ch
                ]
        in assertEqual "" expected actual
    , testCase "Rules with predicates" $
        let expected =
                makeMultipleAndPredicate
                    [ makeMultipleAndPredicate
                        [ makeCeilPredicate_ (mkAnd Mock.cf Mock.b)
                        , makeCeilPredicate_ (Mock.g Mock.a)
                        , makeCeilPredicate_ (Mock.f Mock.b)
                        ]
                    , makeMultipleAndPredicate
                        [ makeCeilPredicate_ (mkAnd Mock.cg Mock.c)
                        , makeCeilPredicate_ (Mock.g Mock.b)
                        , makeCeilPredicate_ (Mock.f Mock.c)
                        ]
                    ]
            actual = mergeRulesPredicate
                [   Pair (Mock.a, makeCeilPredicate_ (Mock.f Mock.a))
                    `rewritesTo`
                    Pair (Mock.cf, makeCeilPredicate_ (Mock.g Mock.a))

                ,   Pair (Mock.b, makeCeilPredicate_ (Mock.f Mock.b))
                    `rewritesTo`
                    Pair (Mock.cg, makeCeilPredicate_ (Mock.g Mock.b))

                ,   Pair (Mock.c, makeCeilPredicate_ (Mock.f Mock.c))
                    `rewritesTo`
                    Pair (Mock.ch, makeCeilPredicate_ (Mock.g Mock.c))
                ]
        in assertEqual "" expected actual
    , testCase "Rules with variables" $
        let expected =
                makeMultipleAndPredicate
                    [ makeCeilPredicate_ (mkAnd (Mock.g x) (Mock.constr11 x_0))
                    , makeCeilPredicate_ (Mock.g x)
                    , makeCeilPredicate_ (Mock.h x_0)
                    ]
            actual = mergeRulesPredicate
                [   Pair (Mock.constr10 x, makeCeilPredicate_ (Mock.f x))
                    `rewritesTo`
                    Pair (Mock.g x, makeCeilPredicate_ (Mock.g x))

                ,   Pair (Mock.constr11 x, makeCeilPredicate_ (Mock.h x))
                    `rewritesTo`
                    Pair (Mock.h x, makeCeilPredicate_ (Mock.h Mock.a))
                ]
        in assertEqual "" expected actual
    , testCase "Three rules case" $
        let expected =
                makeMultipleAndPredicate
                    [ makeCeilPredicate_ (mkAnd Mock.a (mkElemVar Mock.var_x_0))
                    , makeCeilPredicate_ (mkAnd Mock.b (mkElemVar Mock.var_x_1))
                    ]

            actual = mergeRulesPredicate
                [ mkElemVar Mock.x `rewritesTo` Mock.a
                , mkElemVar Mock.x `rewritesTo` Mock.b
                , mkElemVar Mock.x `rewritesTo` Mock.c
                ]
        in assertEqual "" expected actual
    , testCase "Anti Left" $ do
        antiLeft <- parseAntiLeft
            (applyAlias "A"
                (mkOr
                    (applyAlias "B"
                        (mkAnd (mkEquals_ Mock.cf Mock.cg) Mock.ch)
                    )
                    mkBottom_
                )
            )
        let expected =
                makeMultipleAndPredicate
                    [ makeCeilPredicate_ (mkAnd Mock.a (mkElemVar Mock.var_x_0))
                    , makeNotPredicate
                        (makeAndPredicate
                            (makeEqualsPredicate_ Mock.cf Mock.cg)
                            (makeCeilPredicate_ (mkAnd Mock.a Mock.ch))
                        )
                    , makeCeilPredicate_ (mkAnd Mock.b (mkElemVar Mock.var_x_1))
                    ]

            actual = mergeRulesPredicate
                [ mkElemVar Mock.x `rewritesTo` Mock.a
                , mkElemVar Mock.x `rewritesTo` Mock.b
                    `withAntiLeft` antiLeft
                , mkElemVar Mock.x `rewritesTo` Mock.c
                ]
        assertEqual "" expected actual
    ]
  where
    x :: TermLike VariableName
    x = mkElemVar Mock.x
    x_0 :: TermLike VariableName
    x_0 = mkElemVar Mock.var_x_0

test_combineRules :: [TestTree]
test_combineRules =
    [ testCase "One rule" $ do
        let expected = [Mock.a `rewritesTo` Mock.cf]

        actual <- runMergeRules [ Mock.a `rewritesTo` Mock.cf ]

        assertEqual "" expected actual
    , testCase "Two rules" $ do
        let expected = [Mock.a `rewritesTo` Mock.cf]

        actual <- runMergeRules
            [ Mock.a `rewritesTo` Mock.b
            , Mock.b `rewritesTo` Mock.cf
            ]

        assertEqual "" expected actual
    , testCase "Predicate simplification" $ do
        let expected =
                [   Pair
                        ( Mock.a
                        , makeAndPredicate
                            (makeCeilPredicate_ Mock.cf)
                            (makeEqualsPredicate_ Mock.cf Mock.b)
                        )
                    `rewritesTo` Pair (Mock.cg, makeTruePredicate_)
                ]

        actual <- runMergeRules
            [ Mock.a `rewritesTo` Mock.functionalConstr10 Mock.cf
            , Mock.functionalConstr10 Mock.b `rewritesTo` Mock.cg
            ]

        assertEqual "" expected actual
    , testCase "Substitution" $ do
        let expected =
                [   Mock.functionalConstr10 (Mock.functionalConstr11 y)
                    `rewritesTo` y
                ]

        actual <- runMergeRules
            [ Mock.functionalConstr10 x `rewritesTo` x
            , Mock.functionalConstr11 y `rewritesTo` y
            ]

        assertEqual "" expected actual
    , testCase "Substitution in predicates" $ do
        let expected =
                [   Pair
                        ( Mock.functionalConstr10 (Mock.functionalConstr11 y)
                        , makeAndPredicate
                            (makeEqualsPredicate_
                                (Mock.f (Mock.functionalConstr11 y))
                                (Mock.g (Mock.functionalConstr11 y))
                            )
                            (makeEqualsPredicate_
                                (Mock.g (Mock.functionalConstr11 y))
                                (Mock.h (Mock.functionalConstr11 y))
                            )
                        )
                    `rewritesTo` Pair (y, makeTruePredicate_)
                ]

        actual <- runMergeRules
            [   Pair
                    ( Mock.functionalConstr10 x
                    , makeEqualsPredicate_ (Mock.f x) (Mock.g x)
                    )
                `rewritesTo`
                Pair (x, makeEqualsPredicate_ (Mock.g x) (Mock.h x))
            , Mock.functionalConstr11 y `rewritesTo` y
            ]

        assertEqual "" expected actual
    , testCase "renameRulesVariables" $ do
        let original =
                [ Mock.functionalConstr10 x `rewritesTo` x
                , Mock.functionalConstr11 x `rewritesTo` x
                ]
            expected =
                [ Mock.functionalConstr10 x `rewritesTo` x
                , Mock.functionalConstr11 x0 `rewritesTo` x0
                ]
            actual = renameRulesVariables original
        assertEqual "" expected actual
    , testCase "Renames variables" $ do
        let expected =
                [   Mock.functionalConstr10 (Mock.functionalConstr11 x0)
                    `rewritesTo` x0
                ]

        actual <- runMergeRules
            [ Mock.functionalConstr10 x `rewritesTo` x
            , Mock.functionalConstr11 x `rewritesTo` x
            ]

        assertEqual "" expected actual
    ]
  where
    x = mkElemVar Mock.x
    x0 = mkElemVar Mock.var_x_0
    y = mkElemVar Mock.y

test_combineRulesGrouped :: [TestTree]
test_combineRulesGrouped =
    [ testCase "One rule" $ do
        let expected = [Mock.a `rewritesTo` Mock.cf]

        actual <- runMergeRulesGrouped [ Mock.a `rewritesTo` Mock.cf ]

        assertEqual "" expected actual
    , testCase "Two rules" $ do
        let expected = [Mock.a `rewritesTo` Mock.cf]

        actual <- runMergeRules
            [ Mock.a `rewritesTo` Mock.b
            , Mock.b `rewritesTo` Mock.cf
            ]

        assertEqual "" expected actual
    , testCase "Two rules" $ do
        let expected =
                [   Mock.functionalConstr10
                        (Mock.functionalConstr11 (Mock.functionalConstr12 z))
                    `rewritesTo` z
                ]

        actual <- runMergeRules
            [ Mock.functionalConstr10 x `rewritesTo` x
            , Mock.functionalConstr11 y `rewritesTo` y
            , Mock.functionalConstr12 z `rewritesTo` z
            ]

        assertEqual "" expected actual
    ]
  where
    x = mkElemVar Mock.x
    y = mkElemVar Mock.y
    z = mkElemVar Mock.z

applyAlias :: Text -> TermLike VariableName -> TermLike VariableName
applyAlias name aliasRight =
    mkApplyAlias
        Alias
            { aliasConstructor = testId name
            , aliasParams = []
            , aliasSorts = applicationSorts [] Mock.testSort
            , aliasLeft = []
            , aliasRight
            }
        []

runMergeRules
    :: [RewriteRule VariableName]
    -> IO [RewriteRule VariableName]
runMergeRules (rule : rules) =
    runNoSMT
    $ runSimplifier Mock.env
    $ mergeRules (rule :| rules)
runMergeRules [] = error "Unexpected empty list of rules."

runMergeRulesGrouped
    :: [RewriteRule VariableName]
    -> IO [RewriteRule VariableName]
runMergeRulesGrouped (rule : rules) =
    runNoSMT
    $ runSimplifier Mock.env
    $ mergeRulesConsecutiveBatches 2 (rule :| rules)
runMergeRulesGrouped [] = error "Unexpected empty list of rules."
