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
text(appreciationReceipt, "").


% Intent: context mismatch
text(c10, contextMismatch, ""). % we don't care exactly what user said. we got some response.
text(a50recipeSelect, contextMismatch, "").
text(a50recipeConfirm, contextMismatch, "").
text(c40, contextMismatch, "").

% Intent: describeCapability
text(describeCapability, "").
	
% Intent: farewell
text(farewell, "").

% Intent: greeting
text(greeting, "").


% Intent: negative welfare receipt
text(negativeWelfareReceipt, "").

% Intent: paraphrase request
text(c10, paraphraseRequest, ""). % we don't care exactly what user said. we got some response.
text(a50recipeSelect, paraphraseRequest, "").
text(a50recipeConfirm, paraphraseRequest, "").
text(c40, paraphraseRequest, ""). % we don't care exactly what user said. we got some response.

% Intent: positive receipt
text(positiveReceipt, "").
	

% Intent: self identification
%text(selfIdentification, Txt) :- 

% Intent: specify goal
text(specifyGoal, '.').

% Intent: session closer
text(sessionCloser, "").

% Intent: sequence closer
text(sequenceCloser, "").

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
text(confirmRequest, "").

% Intent: feature inquiry
text(featureInquiry, "") :- 
	recipesFiltered(Recipes), length(Recipes, L), L > 890.
	
text(featureInquiry, "") :-
	recipesFiltered(Recipes), length(Recipes, L), L < 891, L > 15,
	not(memoryKeyValue('show', 'true')).
		
text(featureInquiry, "") :-
	recipesFiltered(Recipes), length(Recipes, L), 
	( L<16 ; memoryKeyValue('show', 'true') ).




% Intent: feature removal request
text(featureRemovalRequest, "").


% This intent is used for answering the user intent pictureRequest.
% Intent: grantPicture
%text(grantPicture, "") :- .
% Intent: pictureNotGranted
% request user to provide more preferences.
%text(pictureNotGranted, "" ):- .

% Intent: no recipes left
text(noRecipesLeft, Txt) :-
	recipesFiltered([]),
	getParamsPatternInitiatingIntent(user, addFilter, Params),
	filters_to_text(Params, String),
	string_concat("I added your request for recipes that ", String, Str1),
	string_concat(Str1, ", but I could not find a recipe that matches all of your preferences.", Txt).


% Intent: recipe choice receipt
%text(recipeChoiceReceipt, Txt) :- 	.

% Intent: recipeCheck
text(recipeCheck, "").
	
% Intent: recipe inquiry

text(recipeInquiry, "").

% used in a21removeKeyFromMemory for handling deleteParameter.
text(removedSpecificFilter(DelFilter), Txt) :-
	filter_to_atom('filterType', DelFilter, Str1),
	string_concat("I removed the ", Str1, Str2),
	string_concat(Str2, " requirement.", Txt).


