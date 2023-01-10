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

%ingredients(RecipeID, IngredientList) :-	.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%nrOfIngredients(RecipeID, N) :-.

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
%recipeSteps(RecipeID, StepsList) :-	.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	


getStepString(RecipeID, String) :-
	step(RecipeID, NrStep, StepSentence), 
	atomic_list_concat(['Step ', NrStep, ': ', StepSentence, '.'], String).

%nrSteps(RecipeID, N) :-  .
	


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
%recipes_filtered(RecipeIDsIn, [ ParamName = Value | Filters], RemainingRecipeIDs) :-
%	applyFilter(ParamName, Value, RecipeIDsIn, RecipeIDsOut),
%	recipes_filtered(RecipeIDsOut, Filters, RemainingRecipeIDs).

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
%applyFilter('cuisine', Value, RecipeIDsIn, RecipeIDsOut) :- .
		
%%%
% Predicate to filter recipes that meet dietary restriction (vegetarian etc).
%applyFilter('dietaryrestriction', Value, RecipeIDsIn, RecipeIDsOut) :-	.

%diet(RecipeID, DietaryRestriction) :-	.
	
ingredientsMeetDiet([], _). % the empty list of ingredients meets any dietary restriction!
%ingredientsMeetDiet([ Ingredient | Rest ], DietaryRestriction) :-	.

% Predicate to filter recipes on max amount of time
%applyFilter('duration', Minutes, RecipeIDsIn, RecipeIDsOut) :-	.

%%%
% Predicate to filter on easy recipes
% 
%applyFilter('easykeyword', _, RecipeIDsIn, RecipeIDsOut) :-	.

% A recipe is easy when:
% - they can be made within 45 minutes, 
% - have less than 18 steps, and
% - less than 15 ingredients
%easyRecipe(RecipeID) :-	.
		


%%% 
% Predicate to filter recipes on the exclusion of a specific ingredient 
% Use example: the user wants to filter recipes NOT including "tahini" (where tahini is an
% ingredient)
%applyFilter('excludeingredient', Ingredient, RecipeIDsIn, RecipeIDsOut) :-	.

%applyFilter('excludeingredienttype', Ingredient, RecipeIDsIn, RecipeIDsOut) :-	. 

%%%
% Predicates to filter recipes on a specific ingredient 
% Use example: the user wants to filter recipes including "tahini" (where tahini is an 
% ingredient)
%applyFilter('ingredient', Ingredient, RecipeIDsIn, RecipeIDsOut) :-	.

%applyFilter('ingredienttype', Ingredient, RecipeIDsIn, RecipeIDsOut) :-	.

%%%
% Predicate to filter recipes on meal type (breakfast e.g.)
%applyFilter('mealType', Value, RecipeIDsIn, RecipeIDsOut) :-	.
		
%%% 
% Predicate to filter recipes on maximum number of ingredients, recipe steps, or time.
%applyFilter('nrOfIngredients', Value, RecipeIDsIn, RecipeIDsOut) :-	.

%applyFilter('nrSteps', Value, RecipeIDsIn, RecipeIDsOut) :-	.

% Predicate to filter recipes on max amount of time of fast (under 30 minutes) recipes
%applyFilter('shorttimekeyword', _, RecipeIDsIn, RecipeIDsOut) :-	.

%%%
% Predicate to filter on number of servings 
%applyFilter('servings', Value, RecipeIDsIn, RecipeIDsOut) :-	.

%%%
% Predicate to filter recipes on tag
% Use example: the user wants to filter on "pizza" dishes (recipes that have the "pizza" tag)
%applyFilter('tag', Value, RecipeIDsIn, RecipeIDsOut) :-	.