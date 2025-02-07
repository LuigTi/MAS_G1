%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Responses when a flag has been set for a button.					%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% responses for NEW dialog agent 		   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

flagResponse('Mic', 'Please press Start first') :- flag('Mic'), waitingForEvent('Start').
flagResponse('Mic', 'I am already listening') :- 
	flag('Mic'), listening.
% We might have just stopped listening but still waiting for results from intention
% detection; so case above does not apply but we still need user to be patient. Order of
% these rules therefore is also important.
flagResponse('Mic', 'Wait a second') :-
	flag('Mic'), waitingForEvent('IntentDetectionDone').
flagResponse('Mic', "Please, I'm talking") :- 
	flag('Mic'), talking.
flagResponse('Mic', 'Not available right now') :- flag('Mic'), not(waitingForEvent(_)).
% In all other cases, flags generate an 'empty' response.
flagResponse(_, '').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Text generator that generates something to say from scripted text and phrases for 	%%%
%%% intents that the agent will generate (use).						%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Text generator that takes dialog context into account.
% We use top level dialog context, e.g.: 
% - greeting (c10)
% - recipe selection (a50recipeSelect)
% - recipe choice confirmation (a50recipeConfirm)
% - closing (c40) 
text_generator(Intent, SelectedText) :- 
	currentTopLevel(PatternId),
	findall(Text, text(PatternId, Intent, Text), Texts), random_select(SelectedText, Texts, _).

% Text generator that does not take dialog context into account.
text_generator(Intent, SelectedText) :- 
	findall(Text, text(Intent, Text), Texts), random_select(SelectedText, Texts, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Scripted text and phrases for ** GENERIC ** intents (sorted on intent name)		%%%
%%% Text is only provided for those intents that the agent will generate (use). 	%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Intent: appreciation receipt
text(appreciationReceipt, "You're welcome.").

% ingredientCheck
text(ingredientCheck, "Do you have all the ingredients").

% lastTopicCheck
text(lastTopicCheck, "Would you like to select a different recipe?").

% Intent: context mismatch
text(c10, contextMismatch, "I am not sure what that means in this context."). % we don't care exactly what user said. we got some response.
text(a50recipeSelect, contextMismatch, "could you specify some filters to apply?").
text(a50recipeSelect, contextMismatch, "could you specify some criteria for the recipe selection?").
text(a50recipeConfirm, contextMismatch, "this is an yes no question.").
text(a50recipeConfirm, contextMismatch, "please answer with yes or no.").
text(c40, contextMismatch, "you should farewell me.").
text(c40, contextMismatch, "please say goodbye.").

text(e1, contextMismatch, "this is a yes or no question").
text(e12, contextMismatch, "please specify the day.").
text(e13, contextMismatch, "please specify the meal.").





% Intent: describeCapability
text(describeCapability, "I can query a database to find a recipe you like, I can also add recipes you choose to a weekly calendar and then print out your grocery list based on your planned recipes!").
	
% Intent: farewell
text(farewell, "Bye bye").

% Intent: greeting
text(greeting, "Hello"). 


% Intent: negative welfare receipt
text(negativeWelfareReceipt, "negative welfare receipt sentence").

% Intent: paraphrase request
text(c10, paraphraseRequest, "I did not understand."). % we don't care exactly what user said. we got some response.
text(a50recipeSelect, paraphraseRequest, "what do you mean?").			%%%%%%%%%%%TODO
text(a50recipeConfirm, paraphraseRequest, "Is this right?").
text(c40, paraphraseRequest, "I did not understand."). % we don't care exactly what user said. we got some response.
text(e1, paraphraseRequest, "COuld you answer with yes or no?").
text(e2, paraphraseRequest, "I did not understand").
text(e3, paraphraseRequest, "I did not understand").

% Intent: positive receipt
text(positiveReceipt, "ok!").
	

% Intent: self identification
text(selfIdentification, Txt) :- agentName(Bot_name), string_concat("I am", Bot_name, Txt).

% Intent: specify goal
text(specifyGoal, "I'll help you find a recipe.").

% Intent: session closer
text(sessionCloser, "See you again.").

% Intent: sequence closer
text(sequenceCloser, "sequence closer sentence").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Scripted text and phrases for ** DOMAIN SPECIFIC ** intents (sorted on intent name)	%%%
%%% Text is only provided for those intents that the agent will generate (use). 	%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Intent: acknowledge the filters that user added (there are recipes that satisfy all)
text(ackFilter, Txt) :-
	not(recipesFiltered([])),
	getParamsPatternInitiatingIntent(user, addFilter, Params),
	filters_to_text(Params, TxtPart2),
	string_concat("The remaining recipes ", TxtPart2, Txt).

text(ackFilter, Txt) :-
	not(recipesFiltered([])),
	getParamsPatternInitiatingIntent(user, addFilter, Params),
	filters_to_text(Params, TxtPart2),
	string_concat("All the recipes left ", TxtPart2, Txt).
	
text(ackFilter, Txt) :-
	not(recipesFiltered([])),
	getParamsPatternInitiatingIntent(user, addFilter, Params),
	filters_to_text(Params, TxtPart2),
	string_concat("These recipes ", TxtPart2, Txt).

% Intent: confirm request for specific filter:
text(confirmRequest, "Confirming request").

% Intent: feature inquiry
text(featureInquiry, "You have not applied any filters") :-recipesFiltered(Recipes), length(Recipes, L), L > 890.
	
text(featureInquiry, "There are too many recipes to show") :-recipesFiltered(Recipes), length(Recipes, L), L < 891, L > 15,not(memoryKeyValue('show', 'true')).
		
text(featureInquiry, "I'll show you the recipes") :-recipesFiltered(Recipes), length(Recipes, L),( L<16 ; memoryKeyValue('show', 'true') ).




% Intent: feature removal request
text(featureRemovalRequest, "Could you remove some of the filters?").


% This intent is used for answering the user intent pictureRequest.
% Intent: grantPicture
text(grantPicture, "I'll show you the recipes")  :- recipesFiltered(Recipes), length(Recipes, L), L <100.
% Intent: pictureNotGranted
% request user to provide more preferences.
text(pictureNotGranted, "There are still too many recipes" ):- recipesFiltered(Recipes), length(Recipes, L), L >99.

% Intent: no recipes left
text(noRecipesLeft, Txt) :-
	recipesFiltered([]),
	getParamsPatternInitiatingIntent(user, addFilter, Params),
	filters_to_text(Params, String),
	string_concat("I added your request for recipes that ", String, Str1),
	string_concat(Str1, ", but I could not find a recipe that matches all of your preferences.", Txt).


% Intent: recipe choice receipt
text(recipeChoiceReceipt, Txt) :- currentRecipe(ID), recipeName(ID, RecipeName), recipeIDs(List),  member(ID, List), string_concat("Your recipe is", RecipeName, Txt).

% Intent: recipeCheck
text(recipeCheck, "Look at the recipe, is it fine?").
	
% Intent: recipe inquiry

text(recipeInquiry, "What would you like to cook?").

% used in a21removeKeyFromMemory for handling deleteParameter.
text(removedSpecificFilter(DelFilter), Txt) :-
	filter_to_atom('filterType', DelFilter, Str1),
	string_concat("I removed the ", Str1, Str2),
	string_concat(Str2, " requirement.", Txt).

% testt111
text(questionCalendar, "would you like to add this recipe to the calendar?").

text(questionDay, "on what day would you like to cook this recipe?").

text(questionMeal, "would you like to cook this recipe as breakfast, lunch, or dinner?").

text(followUp, "Would you like to visualize the calendar or look for another recipe?").

%text(nextMove, "would you like to add a new recipe or delete a recipe?").

text(insertDay(_), "Day added.").

text(addCalendar, "would you like to add something to your calendar").

text(insertMeal(_), "Meal added.").

%text(questionIngredient, "Do you want to look at the ingredient again?").

%text(insertIngredient, "Here is the ingredient.").

%text(deleteCalendar, "would you like to delete a recipe from the calendar?").

%text(recipeDelete, "Choose the recipe you want to delete from the calendar.").

%text(deleteRecipe(_), "Recipe deleted").

text(clearMemory, "removing all previous filters").

text(groceryList, "I am hereby making your grocery list").

text(groceryQuestion, "Would you like to see your grocery list?").

text(showGrocery, "This is your grocery list.").


text(questionFinal, "It's been a pleasure to help you out, when you are done with the grocery list, you can just say Bye Bye.").


text(insertMeal(_), "got it!").

text(seeCalendar, "Do you want to access the calendar?").

text(showCalendar, "this is your current calendar").

text(addCalendar, "would you like to add something to your calendar?.").

text(deleteCalendar, "would you like to delete a recipe from the calendar?").

text(recipeDelete, "Choose the recipe you want to delete from the calendar.").

text(deleteRecipe(_), "Recipe deleted").

text(clearMemory, "removing all previous filters").

text(groceryList, "I am hereby making your grocery list").

text(groceryQuestion, "Would you like to see your grocery list?").

text(showGrocery, "This is your grocery list.").

text(questionFinal, "It's been a pleasure to help you out, when you are done with the grocery list, you can just say Bye Bye.").




