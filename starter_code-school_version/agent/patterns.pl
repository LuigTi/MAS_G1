%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Patterns (listed alphabetically on their labels/names)     			%%%
%%%										%%%
%%% A pattern is a list that consists of the pattern name or id as first 	%%%
%%% item and a subsequent list of actor/intent pairs which indicate which 	%%%
%%% intents should follow each other and who is expected to have the turn.	%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Conversational Activity Patterns (Moore Ch5)           %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% A2 Patterns: Open Request

% Pattern A2.4 - Open request Agent Detail Request (focused on recipe)
% Agent to follow-up with detailed request for purpose of slot filling after partial user request
% Example:
%	(U: how much water do I need? --> not part of pattern but listed here to provide context)
%	A: for which recipe?
%	U: for the pasta 

% Generic pattern for slot filling. See definition of slotFill/2 predicate below. 
pattern([slotFill(X), [agent, repeat(X)]]).

/**
 * slotFill(-IntentName, -PatternName)
 *
 * Indicates that PatternName can be used for filling in missing slots for intent IntentName.
 *
 * @param IntentName	Intent name (should match DialogFlow intent name).
 * @param PatternName	Pattern name (should match a pattern listed in this file).
**/
% Dummy slotFill fact added because at least one such fact needs to be defined.
slotFill(dummyP, dummyI).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pattern: a21featureRequest								%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%Variant 1: a21featureRequest
% A variant for when a user wants to add a specific feature while already checking a recipe 
% and there are recipes that satisfy all feature requests, i.e. the agent can acknowledge 
% that there are recipes left.


pattern(_) :- parent(a21featureRequest, a50recipeConfirm),
	getParamsPatternInitiatingIntent(user, addFilter, Params).

%Variant 2: a21featureRequest

%A variant for when a user wants to add a specific filter while already checking a recipe 
% but there are no recipes left that satisfy all feature requests.

pattern(_)
:- parent(a21featureRequest, a50recipeConfirm),
	getParamsPatternInitiatingIntent(user, addFilter, Params).
	
%Variant 3: a21featureRequest
% A variant for when there are recipes that satisfy all feature requests; automatically 
% remove all feature requests that are conflicting with the new request(s).

pattern(_)
:- getParamsPatternInitiatingIntent(user, addFilter, Params).


%Variant 4: a21featureRequest
%A variant for the case where by adding filters there is no recipe left that satisfies all filters.
pattern(_)
:- getParamsPatternInitiatingIntent(user, addFilter, Params).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pattern: a21noMoreFilters								%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Example
%	A: “What other filters would you like to add”
%	U: “I do not want more filters  “
%	A: “Ok. Here is a list of recipes” / “Sorry the list is too long still to show”


% Variant for when there are less than 100 recipes left. 
%Put Pattern Here 


% Variant for when there are still 100 or more recipes left.
%Put Pattern Here 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pattern: a21removeKeyFromMemory							%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Handling deleteParameter intent.
% Variant for when the user wants to delete a specific feature while already checking a 
% recipe.
pattern([a21removeKeyFromMemory,
	[user, deleteParameter], 
	% actually remove the filter from memory
	[agent, removeParam(DelFilter)],
	[agent, removedSpecificFilter(DelFilter)],
	% after informing the user that the feature has been removed, now also remove
	% the ...Del entity from memory again
	[agent, remove([ 'filterType' = DelFilter ])],
	[agent, insert(a50recipeSelect)]])
:-
	parent(a21removeKeyFromMemory, a50recipeConfirm),
	getParamsPatternInitiatingIntent(user, deleteParameter, Params),
	member(filterType = DelFilter, Params).

% Variant for when a user wants to delete a specific feature while they have not selected
% a recipe yet.
pattern([a21removeKeyFromMemory,
	[user, deleteParameter], 
	% actually remove the filter from memory
	[agent, removeParam(DelFilter)],
	[agent, removedSpecificFilter(DelFilter)],
	% after informing the user that the feature has been removed, now also remove
 	% the ...Del entity from memory again
	[agent, remove([ 'filterType' = DelFilter ])], 
	[agent, featureInquiry] ])
:-
	getParamsPatternInitiatingIntent(user, deleteParameter, Params),
	member(filterType = DelFilter, Params).

%%% Handling deleteFilterValue intent.
% Variant for when the user wants to delete a specific feature value (e.g. the ingredient
% type fish) while already checking a recipe.
pattern([a21removeKeyFromMemory, 
	[user, deleteFilterValue],
	% remove all filters with Value as value from memory (note this also removes the
	% ...Del filter)
	[agent, removeValue(Value)],
	[agent, insert(a50recipeSelect)] ])
:-
%	only do this pattern if there is a ...Del entity (request to remove a feature)
	parent(a21removeKeyFromMemory, a50recipeConfirm),
	getParamsPatternInitiatingIntent(user, deleteFilterValue, Params),
	member(_ = Value, Params), not(Value=''), not(Value=[]).
	
% Variant for when a user wants to delete a specific feature value while they have not
% selected a recipe yet.
pattern([a21removeKeyFromMemory, 
	[user, deleteFilterValue],
	% remove the filter from memory
	% by using a don't care symbol we also remove the ...Del parameter
	[agent, removeValue(Value)],
	[agent, featureInquiry] ])
:- 
%	only do this pattern if there is a ...Del intent (request to remove a feautre)
	getParamsPatternInitiatingIntent(user, deleteFilterValue, Params),
	member(_ = Value, Params), not(Value=''), not(Value=[]).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pattern: a50recipeConfirm	             						%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Example:
%	A: Can you confirm that this is the recipe? 
%	U: Yes

%Confirmation
%Put Pattern Here 

%Appreciation
%Put Pattern Here 

%Disconfirmation
%Put Pattern Here 



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pattern: a50recipeSelect	             						%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Example:
%	A: Let us find a recipe together.
%	A: what recipe would you like to cook?
%	U: i'd like to make pasta (or, simply: pasta)
%	A: pasta is a great choice!


%Put Pattern Here 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Sequence Management Patterns (Moore Ch6) (b12, b13, b14, b42)              %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% B1 Patterns: Repair (Agent)
% Pattern B12: Paraphrase Request (Agent)
% Example:
%	U: chicken soup
%	A: what do you mean?

%Put Pattern Here 

% Pattern B13: Out of context dialog move
% For handling intents that are recognized but not expected nor first intent of a pattern.
% Example:
%	A: what recipe would you like to cook?
%	U: goodbye
%	A: not sure what that means in this context.

%Put Pattern Here 

%Pattern B42: Appreciation Receipt

%Put Pattern Here 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Conversation Management Patterns (Moore Ch7) (c10, c30, c40, c43)          		%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% C1 Patterns: Opening (Agent)
% Pattern C10.0: Opening Greeting (Agent)
% Example:
%	A: hello
%	U: hi


%Put Pattern Here 


% Pattern C10.1: Opening Self-Identification (Agent)
% Example:
%	A: hello
%	A: I'm  BellaBot
%	U: hi

%Put Pattern Here 

%%% C30 Patterns: Capabilities
% Pattern C30.0: General Capability Check
% Example:
%	U: what can you do?
%	A: At the moment I can ....

%Put Pattern Here

%%% C40 Patterns: Closing

%Put Pattern Here




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Special Patterns used to only handle button-based interaction			%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pattern([start, [user, button]]) :- 
	getParamsPatternInitiatingIntent(user, button, [button='Start']).