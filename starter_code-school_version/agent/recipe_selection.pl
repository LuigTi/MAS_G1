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

%ingredients(RecipeID, IngredientList) :- setof(IngredientNeeded,ingredient(RecipeID, IngredientNeeded), IngredientList).
ingredients(RecipeID, IngredientList) :- setof(IngredientNeeded,RecipeID^ingredient(RecipeID, IngredientNeeded), IngredientList).

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
%recipeSteps(RecipeID, StepsList) :- findall(RecipeID^getStepString(RecipeID, Step), StepsList). % TODO: JIIPPPPPPP may be problematic with the visual, check it!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
recipeSteps(RecipeID, StepsList) :- bagof(String, RecipeID^getStepString(RecipeID, String), StepsList). % TODO: JIIPPPPPPP may be problematic with the visual, check it!


getStepString(RecipeID, String) :-
	step(RecipeID, NrStep, StepSentence), 
	atomic_list_concat(['Step ', NrStep, ': ', StepSentence, '.'], String).

nrSteps(RecipeID, N) :-  recipeSteps(RecipeID, StepsList), length(StepsList,N).


randomRecipe(RecipeID) :- recipeIDs(RecipesIDs), random_member(RecipeID, RecipesIDs).


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
applyFilter('cuisine', Value, RecipeIDsIn, RecipeIDsOut) :- downcase_atom(Value, StringDown), findall(Recipe,(member(Recipe,RecipeIDsIn), cuisine(Recipe, StringDown)),RecipeIDsOut).		
%%%
% Predicate to filter recipes that meet dietary restriction (vegetarian etc).
applyFilter('dietaryrestriction', Value, RecipeIDsIn, RecipeIDsOut) :- downcase_atom(Value, StringDown), findall(Recipe,(member(Recipe,RecipeIDsIn), diet(Recipe, StringDown)),RecipeIDsOut).

diet(RecipeID, DietaryRestriction) :- ingredients(RecipeID, IngredientList), ingredientsMeetDiet(IngredientList, DietaryRestriction).
	
ingredientsMeetDiet([], _). % the empty list of ingredients meets any dietary restriction!
ingredientsMeetDiet([ Ingredient | Rest ], DietaryRestriction) :- typeIngredient(Ingredient,DietaryRestriction), ingredientsMeetDiet(Rest,DietaryRestriction)	.

% Predicate to filter recipes on max amount of time
applyFilter('duration', Minutes, RecipeIDsIn, RecipeIDsOut) :- findall(Recipe,(member(Recipe,RecipeIDsIn), time(Recipe, Time), Time =< Minutes),RecipeIDsOut).

%%%
% Predicate to filter on easy recipes
applyFilter('easykeyword', _, RecipeIDsIn, RecipeIDsOut) :- findall(Recipe, (member(Recipe,RecipeIDsIn),easyRecipe(Recipe)),RecipeIDsOut).

% A recipe is easy when:
% - they can be made within 45 minutes, 
% - have less than 18 steps, and
% - less than 15 ingredients
easyRecipe(RecipeID) :-	time(RecipeID, Time), Time =< 45, nrSteps(RecipeID, N1), N1 < 18, nrOfIngredients(RecipeID, N2), N2 < 15.
		


%%% 
% Predicate to filter recipes on the exclusion of a specific ingredient 
% Use example: the user wants to filter recipes NOT including "tahini" (where tahini is an
% ingredient)
applyFilter('excludeingredient', Ingredient, RecipeIDsIn, RecipeIDsOut) :- downcase_atom(Ingredient, StringDown), findall(Recipe,(member(Recipe,RecipeIDsIn), not(hasIngredient(Recipe,StringDown))),RecipeIDsOut).

%% TODO: check it, is pretty long
applyFilter('excludeingredienttype', IngredientType, RecipeIDsIn, RecipeIDsOut) :- downcase_atom(IngredientType, IngredientTypeDown), findall(Recipe,(member(Recipe,RecipeIDsIn), findall(Type,(hasIngredient(Recipe,Ingred),typeIngredient(Ingred,Type)), ListTypeOfIngredRecipe),not(member(IngredientTypeDown,ListTypeOfIngredRecipe))),RecipeIDsOut).


%%%
% Predicates to filter recipes on a specific ingredient 
% Use example: the user wants to filter recipes including "tahini" (where tahini is an 
% ingredient)
applyFilter('ingredient', Ingredient, RecipeIDsIn, RecipeIDsOut) :- downcase_atom(Ingredient, StringDown), findall(Recipe,(member(Recipe,RecipeIDsIn),hasIngredient(Recipe,StringDown)),RecipeIDsOut).

applyFilter('ingredienttype', Ingredient, RecipeIDsIn, RecipeIDsOut) :-downcase_atom(Ingredient, StringDown), findall(Recipe,(member(Recipe,RecipeIDsIn), typeIngredient(Ingred, StringDown),hasIngredient(Recipe,Ingred)),RecipeIDsOut).	

%%%
% Predicate to filter recipes on meal type (breakfast e.g.)
applyFilter('mealType', Value, RecipeIDsIn, RecipeIDsOut) :-downcase_atom(Value, StringDown), findall(Recipe, (member(Recipe,RecipeIDsIn), mealType(Recipe, StringDown)), RecipeIDsOut).

%%% 
% Predicate to filter recipes on maximum number of ingredients, recipe steps, or time.
applyFilter('nrOfIngredients', Value, RecipeIDsIn, RecipeIDsOut) :- findall(Recipe,( member(Recipe,RecipeIDsIn), nrOfIngredients(Recipe, N), N=<Value),RecipeIDsOut).

applyFilter('nrSteps', Value, RecipeIDsIn, RecipeIDsOut) :- findall(Recipe,(member(Recipe,RecipeIDsIn), nrSteps(Recipe, N), N=<Value),RecipeIDsOut).

% Predicate to filter recipes on max amount of time of fast (under 30 minutes) recipes
applyFilter('shorttimekeyword',_ , RecipeIDsIn, RecipeIDsOut) :-findall(Recipe,(member(Recipe,RecipeIDsIn), time(Recipe, Time), Time =< 30),RecipeIDsOut).

%%%
% Predicate to filter on number of servings 
applyFilter('servings', Value, RecipeIDsIn, RecipeIDsOut) :- findall(Recipe,(member(Recipe,RecipeIDsIn), servings(Recipe, Value)),RecipeIDsOut).

%%%
% Predicate to filter recipes on tag
% Use example: the user wants to filter on "pizza" dishes (recipes that have the "pizza" tag)
%
applyFilter('tag', Value, RecipeIDsIn, RecipeIDsOut) :- findall(Recipe,(member(Recipe,RecipeIDsIn), tag(Recipe, Value)),RecipeIDsOut).


%% NEW DEFINED BY TLP
applyFilter('nutrients', Nutrient, RecipeIDsIn, RecipeIDsOut) :-downcase_atom(Nutrient, StringDown), 
findall(Recipe,(member(Recipe,RecipeIDsIn), typeIngredient(Ingred, StringDown),hasIngredient(Recipe,Ingred)),RecipeIDsOut).

