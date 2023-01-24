%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 	NEW Knowledge and reasoning base for the recipe selection process 		%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/**
 * chosenRecipe(-RecipeID)
 *
 * Retrieves the chosen recipe from memory (assumes that the last time a recipe is mentioned 
 * also is the user's choice).
 *
 * @param RecipeID Recipe ID.
**/
currentRecipe(RecipeID) :- memoryKeyValue("recipe", RecipeName), recipeName(RecipeID, RecipeName).


/**
 * ingredients(-IngredientList)
 *
 * Retrieves the ingredients from memory (assumes that the last time a recipe is mentioned 
 * also is the user's choice), can be used for presenting the ingredients to the user.
 *
 * @param RecipeID Recipe's ID.
 * @param IngredientList List containing the ingredients with their quantity.

**/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%For Visual Support to-do 

ingredients(RecipeID, IngredientList) :- findall(IngredientNeeded,ingredient(RecipeID, IngredientNeeded), IngredientListNotFinal), list_to_set(IngredientListNotFinal,IngredientList ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



nrOfIngredients(RecipeID, N) :- ingredients(RecipeID,ListIng), length(ListIng,N).

/**
 * steps(-IngredientList)
 *
 * Retrieves the steps from memory (assumes that the last time a recipe is mentioned 
 * also is the user's choice), for presenting the steps to the user.
 *
 * @param RecipeID Recipe name in lower case.
 * @param StepsList List containing the steps of the recipe.

**/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%For Visual Support to-do 
recipeSteps(RecipeID, StepsList) :- findall(StepNeeded,step(RecipeID, _, StepNeeded), StepsList). % TODO: JIIPPPPPPP may be problematic with the visual, check it!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	


getStepString(RecipeID, String) :-
	step(RecipeID, NrStep, StepSentence), 
	atomic_list_concat(['Step ', NrStep, ': ', StepSentence, '.'], String).

nrSteps(RecipeID, N) :-  recipeSteps(RecipeID, StepsList), length(StepsList,N).
	


/**
 * recipeIDs(-Recipes)
 *
 * Collects all recipe IDs from the recipe database using the recipeID/1
 * predicate.
 *
 * @param RecipesIDs a list of (shorthand) recipe IDs available in this file.
**/

recipeIDs(RecipesIDs) :- setof(RecipeID, recipeID(RecipeID), RecipesIDs).

/**
 * recipesFiltered(-RecipesIDs)
 *
 * Collects all recipe IDs from the recipe database that have been filtered with the available
 * filters used so far.
 *
 * @param Recipes A list of recipe IDs filtered with the used filters.
**/
% recipesFilteredNew(-RecipeIDs):
% retrieves all recipes that are filtered with the currently active feature selections 
% this is done by retrieving all recipes and the memory, filtering the memory by feature selection parameters
% and then recursively filter all recipes on the filters.
recipesFiltered(RecipeIDs) :-
	recipeIDs(RecipeIDsAll),
	filters_from_memory(Filters),
	recipes_filtered(RecipeIDsAll, Filters, RecipeIDFiltered),
	% because we used findall (which never fails but may return duplicates) we need
	% to remove any such duplicates next. 
	list_to_set(RecipeIDFiltered, RecipeIDs).

% Recursively go through all user provided features to find those recipes that satisfy all
% of these features.  
recipes_filtered(RecipeIDs, [], RecipeIDs).
recipes_filtered(RecipeIDsIn, [ ParamName = Value | Filters], RemainingRecipeIDs) :-
	applyFilter(ParamName, Value, RecipeIDsIn, RecipeIDsOut),
	recipes_filtered(RecipeIDsOut, Filters, RemainingRecipeIDs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% recipe selection features  %%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
 * applyFilter(+ParamName, +Value, +RecipeIDs, -FilteredRecipes)
 *
 * Filters the recipes provided as input using the (Key) feature with associated value and
 * returns the recipes that satisfy the feature as output.
 *
 * @param ParamName A parameter name referring to a feature that the recipes should have.
 * @param Value The associated value of the feature (parameter name).
 * @param RecipeIDs The recipes that need to be filtered.
 * @param FilteredRecipes The recipes that have the required feature.
**/

%%%
% Predicate to filter recipes on cuisines (e.g., Italian recipes)
applyFilter('cuisine', Value, RecipeIDsIn, RecipeIDsOut) :- findall(Recipe,check_cuisine_from_list(Recipe, RecipeIDsIn, Value),RecipeIDsOut).
check_cuisine_from_list(Recipe, RecipeList,TypeOfCusine):- member(Recipe,RecipeList), cuisine(Recipe, TypeOfCusine).		
%%%
% Predicate to filter recipes that meet dietary restriction (vegetarian etc).
applyFilter('dietaryrestriction', Value, RecipeIDsIn, RecipeIDsOut) :-	findall(Recipe,check_dietary_from_list(Recipe, RecipeIDsIn, Value),RecipeIDsOut).
check_dietary_from_list(Recipe, RecipeList,DietaryRestriction):-member(Recipe,RecipeList), diet(Recipe, DietaryRestriction).


diet(RecipeID, DietaryRestriction) :- ingredients(RecipeID, IngredientList), ingredientsMeetDiet(IngredientList, DietaryRestriction)	.
	
ingredientsMeetDiet([], _). % the empty list of ingredients meets any dietary restriction!
ingredientsMeetDiet([ Ingredient | Rest ], DietaryRestriction) :- typeIngredient(Ingredient,DietaryRestriction), ingredientsMeetDiet(Rest,DietaryRestriction)	.

% Predicate to filter recipes on max amount of time
applyFilter('duration', Minutes, RecipeIDsIn, RecipeIDsOut) :- findall(Recipe,check_time_from_list(Recipe, RecipeIDsIn, Minutes),RecipeIDsOut).
check_time_from_list(Recipe, RecipeList, Value):- member(Recipe,RecipeList), time(Recipe, Time), Time =< Value.

%%%
% Predicate to filter on easy recipes
% 

% A recipe is easy when:
% - they can be made within 45 minutes, 
% - have less than 18 steps, and
% - less than 15 ingredients
easyRecipe(RecipeID) :-	time(RecipeID, Time), Time =< 45, nrSteps(RecipeID, N1), N1 < 18, nrOfIngredients(RecipeID, N2), N2 < 15.
		


%%% 
% Predicate to filter recipes on the exclusion of a specific ingredient 
% Use example: the user wants to filter recipes NOT including "tahini" (where tahini is an
% ingredient)
applyFilter('excludeingredient', Ingredient, RecipeIDsIn, RecipeIDsOut) :- findall(Recipe,check_noingredient_from_list(Recipe, RecipeIDsIn, Ingredient),RecipeIDsOut).
check_noingredient_from_list(Recipe, RecipeList, Ingredient):- member(Recipe,RecipeList), ingredients(Recipe, IngredientList), not(member(Ingredient,IngredientList)).

%% TODO: check it, is pretty long
applyFilter('excludeingredienttype', Ingredient, RecipeIDsIn, RecipeIDsOut) :-findall(Recipe,check_noType_from_list(Recipe, RecipeIDsIn, Ingredient),RecipeIDsOut).
check_noType_from_list(Recipe, RecipeList, Ingredient):- member(Recipe,RecipeList), ingredients(Recipe, IngredientList),from_ingredientList_to_typeList(IngredientList, TypeList), not(member(Ingredient,TypeList) 

from_ingredientList_to_typeList([], List).
from_ingredientList_to_typeList([Ing|List], TypeList):- typeIngredient(Ing,Ty), append(TypeList,[Ty], FinalTypeList),from_ingredientList_to_typeList(List,FinalTypeList)   .
 % im dumb


%%%
% Predicates to filter recipes on a specific ingredient 
% Use example: the user wants to filter recipes including "tahini" (where tahini is an 
% ingredient)
applyFilter('ingredient', Ingredient, RecipeIDsIn, RecipeIDsOut) :- findall(Recipe,check_ingredient_from_list(Recipe, RecipeIDsIn, Ingredient),RecipeIDsOut)	.
check_ingredient_from_list(Recipe, RecipeList, Ingredient):- member(Recipe,RecipeList), ingredients(Recipe, IngredientList),from_ingredientList_to_typeList(IngredientList, TypeList), member(Ingredient,TypeList) .

applyFilter('ingredienttype', Ingredient, RecipeIDsIn, RecipeIDsOut) :-findall(Recipe,check_Type_from_list(Recipe, RecipeIDsIn, Ingredient),RecipeIDsOut).	
check_Type_from_list(Recipe, RecipeList, Ingredient):- member(Recipe,RecipeList), ingredients(Recipe, IngredientList),from_ingredientList_to_typeList(IngredientList, TypeList), member(Ingredient,TypeList) .

%%%
% Predicate to filter recipes on meal type (breakfast e.g.)
applyFilter('mealType', Value, RecipeIDsIn, RecipeIDsOut) :-findall(Recipe,check_mealType_from_list(Recipe, RecipeIDsIn, Value),RecipeIDsOut).
check_mealType_from_list(Recipe, RecipeList,Value):-member(Recipe,RecipeList), mealType(Recipe, Value).

%%% 
% Predicate to filter recipes on maximum number of ingredients, recipe steps, or time.
applyFilter('nrOfIngredients', Value, RecipeIDsIn, RecipeIDsOut) :-findall(Recipe,check_nrOfIngredients_from_list(Recipe, RecipeIDsIn, Value),RecipeIDsOut).
check_nrOfIngredients_from_list(Recipe, RecipeList,Value):-member(Recipe,RecipeList), nrOfIngredients(Recipe, N), N=<Value.

%applyFilter('nrSteps', Value, RecipeIDsIn, RecipeIDsOut) :-findall(Recipe,check_nrsteps_from_list(Recipe, RecipeIDsIn, Value),RecipeIDsOut).
check_nrsteps_fromlist(Recipe, RecipeList,Value):-member(Recipe,RecipeList), nrSteps(Recipe, N), N=<Value.

% Predicate to filter recipes on max amount of time of fast (under 30 minutes) recipes
applyFilter('shorttimekeyword',_ , RecipeIDsIn, RecipeIDsOut) :-findall(Recipe,check_fast_from_list(Recipe, RecipeIDsIn),RecipeIDsOut).
check_fast_from_list(Recipe, RecipeList):-member(Recipe,RecipeList), time(Recipe, Time), Time =< 30.

%%%
% Predicate to filter on number of servings 
applyFilter('servings', Value, RecipeIDsIn, RecipeIDsOut) :- findall(Recipe,check_serving_from_list(Recipe, RecipeIDsIn, Value),RecipeIDsOut).
check_serving_from_list(Recipe, RecipeList,Value):- member(Recipe,RecipeList), serving(Recipe, N), N = Value.

%%%
% Predicate to filter recipes on tag
% Use example: the user wants to filter on "pizza" dishes (recipes that have the "pizza" tag)
%
applyFilter('tag', Value, RecipeIDsIn, RecipeIDsOut) :- findall(Recipe,check_tag_from_list(Recipe, RecipeIDsIn, Value),RecipeIDsOut).
check_tag_from_list(Recipe, RecipeList,Value):-member(Recipe,RecipeList), tag(Recipe, Value).
