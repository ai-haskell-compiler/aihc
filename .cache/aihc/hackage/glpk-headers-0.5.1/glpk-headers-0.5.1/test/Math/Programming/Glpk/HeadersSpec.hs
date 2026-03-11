module Math.Programming.Glpk.HeadersSpec where

import Control.Monad
import Foreign.C.Types
import Foreign.Ptr
import Math.Programming.Glpk.Header
import Test.Hspec
import Text.Printf

spec :: Spec
spec =
  describe "GPLK bindings" $ do
    it "can solve the diet problem" basicDiet

data Food = Corn | Milk | Bread
  deriving
    ( Eq,
      Ord,
      Read,
      Show
    )

data Nutrient = Calories | VitaminA
  deriving
    ( Eq,
      Ord,
      Read,
      Show
    )

basicDiet :: IO ()
basicDiet =
  let cost :: Food -> CDouble
      cost Corn = 0.18
      cost Milk = 0.23
      cost Bread = 0.05

      nutrition :: Nutrient -> Food -> CDouble
      nutrition Calories Corn = 72
      nutrition VitaminA Corn = 107
      nutrition Calories Milk = 121
      nutrition VitaminA Milk = 500
      nutrition Calories Bread = 65
      nutrition VitaminA Bread = 0

      foods :: [Food]
      foods = [Corn, Milk, Bread]

      nutrients :: [Nutrient]
      nutrients = [Calories, VitaminA]

      maxServings :: CDouble
      maxServings = 10

      nutrientBounds :: Nutrient -> (CDouble, CDouble)
      nutrientBounds Calories = (2000, 2250)
      nutrientBounds VitaminA = (5000, 50000)

      numAmountVariables :: CInt
      numAmountVariables = fromIntegral $ length foods

      numNutrientConstraints :: CInt
      numNutrientConstraints = fromIntegral (length nutrients)

      expected :: Food -> CDouble
      expected Corn = 1.94
      expected Milk = 10
      expected Bread = 10

      expectedCost :: CDouble
      expectedCost = 3.15
   in do
        _ <- glp_term_out glpkOff
        problem <- glp_create_prob

        -- Create the decision variables
        amountStart <- glp_add_cols problem numAmountVariables
        let amounts = zip foods [amountStart ..]

        -- Create the nutrient constraints
        nutrientConStart <- glp_add_rows problem numNutrientConstraints
        let nutrientConstraints = zip nutrients [nutrientConStart ..]

        -- Set the objective
        glp_set_obj_dir problem glpkMin
        forM_ amounts $ \(food, column) ->
          glp_set_obj_coef problem column (cost food)

        -- Set the right-hand side of the nutrient constraints
        forM_ nutrientConstraints $ \(nutrient, row) -> do
          let (lower, upper) = nutrientBounds nutrient
          glp_set_row_bnds problem row glpkBounded lower upper

        -- Set the body of the nutrient constraints
        forM_ nutrientConstraints $ \(nutrient, row) ->
          allocaGlpkArray (map snd amounts) $ \indices ->
            allocaGlpkArray (map (nutrition nutrient . fst) amounts) $ \coefs ->
              glp_set_mat_row problem row numAmountVariables indices coefs

        -- Ensure we don't have too many servings of any one food
        forM_ amounts $ \(_, column) ->
          glp_set_col_bnds problem column glpkBounded 0 maxServings

        -- Check that we reached optimality
        status <- glp_simplex problem nullPtr
        status `shouldBe` glpkSimplexSuccess

        -- Check the variable values
        forM_ amounts $ \(food, column) -> do
          x <- glp_get_col_prim problem column
          abs (x - expected food) `shouldSatisfy` (<= 1e-1)

        -- Check the objective value
        objective <- glp_get_obj_val problem
        abs (objective - expectedCost) `shouldSatisfy` (<= 1e-1)
