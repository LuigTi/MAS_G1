%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% This page handles dynamic visuals for your agent!					%%%
%%%											%%%
%%% This file is organized as follows:							%%% 
%%% 1. Page layouts (order of pages listed below matters!)				%%%
%%% 2. Code for HTML generation								%%%
%%% 3. Some Helper predicates								%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic
	% predicate to keep track of whether page is up to date
	pageUpToDate/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Page layouts									%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%% Page layout for bot instruction page (before conversation has started).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Page layout for bot instruction page                    %%%
%%% This Page Should Contain:				    %%%
%%% 1. Instructions for your bot 			    %%%
%%% 2. Start Button      				    %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

page(start, _, Html) :- 
	% Condition for when to show this page
	currentTopLevel(start), 
	% Constructing the HTML page
	% First row: Instructions
	atomic_list_concat([
	"<center><h1> Instructions Go Here </h1></center></br>",
	"<center><p> Please Fill </p></center>"
	], Txt),
	applyTemplate('<div class="row justify-content-center"><div class="alert alert-dark">~a</div></div>', Txt, FirstRow), 
	% Second row: Button Instructions
	SecondRow = '</br></br></br><div class="row justify-content-center""><h3>Press Start to Begin</h3></div>',
	% Third Row: Start Button
	startButton('Start', B),
	applyTemplate('<div class="row justify-content-center">~a</div>', B, ThirdRow),
	% Putting everything together
	atomic_list_concat([FirstRow, SecondRow, ThirdRow], Body), 
	% Create the HTML page
	html(Body, Html).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Page layout for opening/greeting (pattern c10)          %%%
%%% This Page Should Contain:				    %%%
%%% 1. Greet user 					    %%%
%%% 2. Introduce your bot				    %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%page(c10, _, Html) :- .



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Page layout (1) for long recipe selection (pattern a50recipeSelect)%%%
%%% Page shown while there are more than K(=15) recipe options         %%%
%%% This Page Should Contain:				    	       %%%
%%% 1. Show how many recipes are still remaining		       %%%
%%% 2. show which features have been applied for filtering	       %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%page(a50recipeSelect, _, Html) :-

	% Conditions for when to show this page
	
	% Constructing HTML page
%	not(memoryKeyValue('show', 'true')),
%	(memoryKeyValue(Key, _), is_filter_param(Key) -> 
%		atomic_list_concat(['There are ', N, ' recipes available to choose from.'], RecipesLeftStr)
%	),
%	applyTemplate('<div class="alert alert-light"></br></br><center><h1>~a</h1></center></br></br></br></div>', RecipesLeftStr, FirstRow),
%	% Second row: display the filters stored in memory and show them as list on screen.
%	filters_to_strings(FilterStrings),
%	itemsList(FilterStrings, TR),
	% Third row: display text
%	SecondRow = '</br><center><h4>What kind of recipe are you looking for?</h4></center></br>',
%	atomic_list_concat(['<center>', TR, '</center>'], ThirdRow),
	% Putting everything together
%	atomic_list_concat([FirstRow, SecondRow, ThirdRow], Body),
	% Create the HTML page
%	html(Body, Html).
	
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Page layout (2) for short recipe selection with pictures (pattern a50recipeSelect)%%%
%%% Page shown while there are less than or equal to K(=15) recipe options            %%%
%%% This Page Should Contain:				    	      		      %%%
%%% 1. display the features used to select the recipes shown		      	      %%%
%%% 2. display the remaining recipe names and their pictures	                      %%%
%%% OR display a statement that there are no recipes left			      %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%page(a50recipeSelect, _, Html) :-
	% Conditions for when to show this page


	% Constructing HTML page
	
	% First row: Either you have found recipes or no recipe statement
%	(N > 0 ->
%		FirstRow = "<center><h3>I have found the following recipes that you might like:</h3></center>" ;
%		FirstRow = "<center><h3>There are no recipes that meet all of your preferences:</h3></center>"
%	),
	% Second row: display the filters stored in memory and show them as list on screen.
%	filters_to_strings(FilterStrings),
%	itemsList(FilterStrings, SR),
%	atomic_list_concat(['<center>', SR, '</center>'], SecondRow),
	% Third row: list of recipes with pictures
%	buttonPictureList(Recipes, L),
%	applyTemplate('<div class="row justify-content-center">~a</div>', L, ThirdRow),
	% Putting everything together
%	atomic_list_concat([FirstRow, SecondRow, ThirdRow], Body),
	% Create the HTML page
%	html(Body, Html).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Page layout for confirming recipe choice (pattern a50recipeConfirm)%%%
%%% Page shown while there are more than K(=15) recipe options         %%%
%%% This Page Should Contain:				    	       %%%
%%% 1. Show recipe details 		    			       %%%
%%%     - name 		    	    				       %%%
%%%     - picture 		    	  			       %%%
%%%     - serving size, duration 			    	       %%%
%%%     - ingredients 		    	     			       %%%
%%%     - steps		      					       %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%page(a50recipeConfirm, _, Html) :-
	% Condition for when to show this page
	
	
	% Constructing HTML page

	% Name
	
	%Here you should retrieve the chosen recipe and its name

	
%	to_upper_case(Name, TxtUp),
 %	applyTemplate('<div class="card-body"><center><h3 class="card-title">~a</h3></center>', TxtUp, IT),
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Duration
	%%Here you should retrieve the time it takes to do each recipe


%	atomic_list_concat(['Takes ', Minutes, ' minutes'], Time),
%	applyTemplate('<h6 class="text-center"><span class="badge badge-light">~a</span></h6>', Time, D), %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
	%Servings
	%%Here you should retrieve the number of servings the recipe has
	
	
%	string_concat("Serves ", Persons, Servings),
%	applyTemplate('<h6 class="text-center"><span class="badge badge-light">~a</span></h6>', Servings, S), %%%
% 	atom_concat(D, S, DS),
%	atom_concat(IT, DS, TIDS),
%	applyTemplate('<div class="card mb-3">~a</div></div>', TIDS, Row1Html), 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
	%Steps
	%%Here you should retrieve a list of the recipe steps

	
%	append(["<h4>Recipe instructions:</h4>"], Steps, RI),
%	itemsList(RI, Row2Col1Html),
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
	%Ingredients
	%%Here you should retrieve the list of ingredients

%	bulletList(Ingredients, I),
%	atom_concat("<h4>Ingredients:</h4>", I, Row2Col2Html),
%	atom_concat(Row2Col2Html, Row2Col1Html, Row2ColHtml), 
%	applyTemplate('<div class="card mx-auto" style="width:90vw, height:100vw">~a</div>', Row2ColHtml, Row2Html),
	% Putting everything together
%	atomic_list_concat([Row1Html, Row2Html], Body),
	% Create the HTML page
%	html(Body, Html).

	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Page layout for closing/goodbye (pattern 40)            %%%
%%% This Page Should Contain:				    %%%
%%% 1. Good bye 					    %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Thank you and Salutations page at the end.
%page(c40, _, Html) :-.
	




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Code for HTML generation that is provided:						%%%
%%% These function can be used or changed as you would like				%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% bullet point list 
bulletItem(Item, Html) :- applyTemplate('<li>~a</li>', Item, Html).
bulletList(Items, Html) :-
	maplist(bulletItem, Items, ItemsHtml),
	atomic_list_concat(ItemsHtml, Bullets),
	applyTemplate('<ul>~a</ul>', Bullets, Html).


% A button template
button('<button class="btn btn-light btn-lg mt-5 ml-3" style="font-family: "Times New Roman", font-size:1.5rem, height=2vw">~a</button>').
button(Content, Html) :- button(Template), applyTemplate(Template, Content, Html).


% A start button template
startButton('<button class="btn btn-dark btn-lg mt-5 ml-3" style="font-family: "Times New Roman", font-size:1.5rem, height=2vw">~a</button>').
startButton(Content, Html) :- startButton(Template), applyTemplate(Template, Content, Html).



% turns a recipe id list into a list of buttons with pictures of recipes
buttons(ContentList, Html) :- buttons(ContentList, '', Html).
buttons([], Html, Html).
buttons([Curr | Rest], Tmp, Html) :- 
	button(Curr, B), atom_concat(Tmp, B, New), buttons(Rest, New, Html).
	
buttonPictureList(RecipeIDList, Html) :-
	maplist(buttonPictureContent, RecipeIDList, Output), buttons(Output, Html).

buttonPictureContent(ID, Html) :- 
	picture(ID, Url),
	recipeName(ID, Txt),
	image(Url, Txt, Image),
	applyTemplate('<p class="text" style="font-size:15pt;font-family: "Times New Roman"; width: 14rem; text-center">~a</p>', Txt, Paragraph),
	atom_concat(Image, Paragraph, Html).


% Inputting images
image(ImgLink, DataValue, Html) :-
	% create template with data value
	applyTemplate('<img class="card-img-top" data-value="~a" style="width: 14rem;" src=~a>', DataValue, ImgLink, Html).

image2(ImgLink, DataValue, Html) :-
	% create template with data value
	applyTemplate('<img class="card-img-top" data-value="~a"  alt="Card image cap" src=~a>', DataValue, ImgLink, Html).

% List needs to be list of strings, atoms, or integers.
itemsList('<ul class="list-group list-group-flush">~a</ul>').
itemsList(List, Html) :-
	maplist(listItem, List, Output), atomic_list_concat(Output, String),
	itemsList(Template), applyTemplate(Template, String, Html).
listItem(Txt, Html) :- applyTemplate('<li class="list-group-item">~a</li>', Txt, Html).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Code for HTML generation that is provided:						 %%%
%%% It is not recommended to change these functions as they are the backbone of the page %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Code for main html page generation, including bg, header, main, and footer
html(MainContent, Html) :-
	bg(B),
	% Header
	% Check if the mic is flagged and show corresponding text.
	flagResponse('Mic', FlagTxt),
	header(Header),
	applyTemplate(Header, FlagTxt, H),
	% Body of the page
	main(MainTemplate), applyTemplate(MainTemplate, MainContent, M),
	% Footer
	% Show transcript, if any, in the footer.
	( transcript(Transcript) -> T = Transcript ; T = '' ),
	footer(FooterTemplate),
	applyTemplate(FooterTemplate, T, F),
	% Putting everything together.
	atomic_list_concat([B, H, M, F], Html).

%%% Main page layout
bg('<div class="audioEnabled"></div>').
% Header
% We show a listening icon (mic) which can be used as button by a user to switch
% between not listening (muted) and listening.
% listening icon
header('<nav class="navbar mb-1"><button class="btn"><img class="img-fluid" style="width:4rem" data-value="Mic" src="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAOEAAADhCAYAAAA+s9J6AAATzklEQVR4Xu2dX2gcxx3Hx4TUCo6jQgNqnDzINLp7cX3Yobo8CD0YyeAgsPQgG4RlPwRMoz/F8YsigiMUExS/OAZLgqhJIJYraIQjGQQGy+RB6CGnEge5frlzi1WSOA2kUMUR2A1hy+90o6xOd9qZ2dn93d5+DSJyPLsz8/19P/ub2Z2d3SHwp6IVyGQyzrff/lu0tx/Vbufs7HXx3HO/Fel0eof2wTggNAUQnNCkVqtodva6YwKc2tmFmJubE21tbYi7qmAhlEMwQhB5uyqy2ayTTCbZWpHNZkUymYQP2CIgBMRnEn9y8qrT3X2Cqfat1U5OXhXd3SfgB4aIQPTwRXfCr1K7RvhCWzLzAyC2uXa6R0YBvuI+wR+6UTYoD5ENRNM8JIrwAUbNIPspDgj9qLfNscvLy04qlQro7OGfdnl5WaRSKfglAOkhagCiCiGqIfuVUwaesewZCGpR0JWVFae+vt7iGSvzVLlcTiQSCXjHUnggpCUh5+bmnLa2Nktnq/zT4KG/vRgBQjtaVvPw00sheMhLIY9/h4A+Bazy+Z+qOvCRqlIlykE8H+IBwE3iwUuGXoJwhsIBwJLCwU8GfoJoBqIBwG1Fg6c0PQXBNAUDgEqCwVdKMq0XglgaYlXamw8aTQ+1KN7I0JMbECrqVW3L0BS7bVwMy9zUpQOE6lrF+VmgukqbS8JfCspBJAWRMA9UE6lMKXjMQz4I5CFQ3Jaj+cKtxMFY3uatKCD01gjDUG+NvErAZ9soBHG2tw8A9MJL/d/hNYzX1d1CJXE3VE8vr9KZTAb7nwJCL5ts+XdkQW3JPA9ANiwhEUQpIcri4qLT1NTk6SgU0FNgcXFRNDU1wXNFskGQ0j5CFtTjS6c0PAcIt/cL5oI6POmXxUqarZrhqrRVE2RBfbZ0j4DvXIpBDECoC5CN8vAdICzto+npaaezs9OGyXCObRSYnp4WnZ2dALGgEYTYbBYMRcO7fMB7gHCz2+KyZ2h4jHnWBAgB4WaTBP1xTk9LxqwAfUW4vf0oQMSb9Zucj6Fo+BcCQAgIAWH43G2qERACwnVD4AE9D4p4cL+uO65EQgg8muCBEI8qAKHbeZgP8nCIRIBMuOE8QAgI2RTAcHRdekDIZkFMiQAhIOTDD1Mi3JhxuQ+ZkA/F2CeC2AuA5Wp89FHN+PQ2HlGIbDbrJJNJXifGuPZsNiuSyWSsk0GsO48H9fz044E9MqHIZDJOOp3md2NMW4CtEAEhIGSGHxACQkAICJkVAISAkNmCyISAEBACQmYFACEgZLYgMiEgBISAkFkBQAgImS2ITAgIASEgZFYAEAJCZgsiEwJCQAgImRUAhICQ2YLIhIAQEAJCZgUAISBktiAyISAEhICQWQFACAiZLYhMCAgBISBkVgAQAkJmCyITAkJACAiZFQCEgJDZgsiEgBAQAkJmBQAhIGS2IDIhIASEgJBZAUAICJktiEwICAEhIGRWABACQmYLIhMCQkAICJkVAISAkNmCyISAEBACQmYFACEgZLYgMiEgBISAkFkBQAgImS2ITAgIASEgZFYAEAJCZgsiEwJCQAgImRUAhICQ2YLIhIAQEAJCZgUAISBktiAyISAEhICQWQFACAiZLYhMCAgBISBkVgAQAkJmCyITAkJACAiZFQCEgJDZgsiEgBAQAkJmBQAhIGS2IDIhIASEgJBZAUAICJktiEwICAEhIGRWABACQmYLIhMCQkAICJkVAISAkNmCyISAEBACQmYFACEgZLYgMiEgBISAkFkBQAgImS2ITAgIASEgZFYAEAJCZgsiEwJCQAgImRUAhICQ2YLIhIAQEAJCZgUAISBktiAyISAEhICQWQFACAiZLYhMCAgBISBkVgAQAkJmCyITAkJACAiZFQCEgJDZgsiEgBAQAkJmBQAhIGS2IDIhIASEgJBZAUAICJktiEwICAEhIGRWABACQmYLIhMCQkAICJkVAISAkNmCyISAEBACQmYFAKFYXl52UqkUeyDi2oDl5WWRSqV2xLX/1O9Yd54EAIS89geEgFDkcjknkUjwOjHGtedyOZFIJGKdDGLdefL+ysqKU19fH2MMeLu+srIi6uvrY+3DWHfeZT+H14qxrj32Hoy9AAX7A0K+60DsPRhpARYXF53791fE6up/RV1dHc0tTO+0AcIIQUg302gu+d1334na2l+LRKJBpNPpyHo5kg2/desz58yZM+Lu3TtbrNPc3CwuX76sCyMgjACEBF9/f79YWFjY0lq6AH/00Ueiqakpcp6OXIMvXLjgDAwMbARhx44nNn53nJ83fp+cvCq6u08o9W9y8qrT3X2Cz4YxrdlPjMrF/cKFC2JgYEAp7pUie6QaOz097XR2dua1cwehWEwJo+ozKBrWNjU1VUpMYtOOxcVFpczlfparEvfp6WnR2dkZGW9HpqHkzF27nnHW1n7YFkDpYAKxtfWwmJ+/qdpHDEnDx18pNq2th535+ZvKca+trRWrq6tK5w6/y1trjExD5+bmnLa2NqVAUDd1syEdUgkBiVkbPP2nmgXdulHs5+bmRFtbm+f5K0HvSDSShBocHHRGRkaUIZQgjo+Pi56eHpV+AsIQHak6HxwfH3d6enq04z44OChGRkZU4h5ir0tXFYlGUtNPnz7tTExMaAdDdaKONaThelF1vi5vxG03Fyx1T+D06dNiYmIiEv6ORCNJ5N7eXmdsbEwbwuHhYTE0NKTaT2TD8FhUisnw8LAzNDSkHffe3l4xNjamVEd4XY54JjQdjtLzxEuXLqkGAxCG50ilmJw5c8a5dOmSNoQYjgYQyPPn33HOnXtTOxgdHR1iZmZGKeB4oyKAwJU4pc6i7Y6ODmdmZkY77pojoHA6XqYWJXOytrBQ+cWL7zlnz76uHYwjR46IGzdu6PQT2TD4gCvH48iRI86NGze0437x4nvi7NnXlesJvsvla4hEI6n5pnfJ9u3bT8vblPtJS+JaWg5xxqSq67516zPR0nJIOR779u13aHmi7o0Zjbvi7Hori8HdUrlaRjcYhXbr9hPZMLiAG8VCN+5RWjWjK0hwofE4s8lDW/msUGcOQsdgLWkwYVZ9Nihrly9c6wJIx6s+Agmmp3pnjQyEhW7lM5RuUHSHQO669OREaQ8FtPwm1/Tqxttw9MMWPC1R2FpZqHjv3r3O/fv3tSHUvQJTde7F4tz9rob6Z2evi/b2o1p+kyMSXQj37HlePHjwjVZdnBpHpqEkks5CXikqrSPs6uoSU1NTJn3F3NCeO7X17+rqcqamprQvupoL9+310PBM2sIY1mPlMNMHt3QldZyftfuKpWxWwuZnfmY0/dBcoGGnkz7Oom1MH3X5PtR0eEIV+9huHdnQd+T097fNZDJOOp3O16w7HDWZfvjvovkZIgWhn8CcP/+OOHfuTdP+AsSQPWa6QsrnBde8lz6ONDWljyr9Hbpnz/POgwffaF8d/UzWMSw1i5mfxwR1dXUObeSkmwX9xNmsl/6PihyEJq80kUx0g8bHkBTPDjW95mdIKEc8OgDKGEfpFSYpaeQgNFk5IwPkc0iaP42mF+Nc3NhbJq8vyRhHaaVMZCF0b1uvc6V07cRmbI6CaADR+9JiRWOT+OqujvLuSvAl/IoVfAtL1NDY2OgsLS1pzRfkldLSwl6AWD7yvjxlcgdcxraxsVEsLS35qp/D0JFrMIlk8kaFDFRBZBv9BohbHWtNV50saPkCGzqHNkQLvdGmQ1Kb8wZ8zWlz2G0MA/3M96k1NtoQupmj/JFQkzeuJYS0ZXoul7N1AUJGtPSdy0Qikf/GhEkW1NlBgQO07eq0ZcTQ+zU7e91pbz+qHTAJououbIodizOIVjxksquajA3ddDNZIK4Y28CLWREw8FaWqUBnR273KeSd0mw2K5LJpC0N4giiFe3ce/uYZMFdu54Ra2s/WGkLh5cj23ASy2TfGffVM4AhTJxAtOYd06mFHNWMjo6Kvr4+a+0JG8TINlwKVVtb66yurhoPSy09stiIWzabdZLJZNhxDK0+29+YN73TLQGM2ncnSgUq8hD6nUuQKJaHpRvJNjQywqvIql/cFyzdYWhAc/vwlHTVZFVUlh6sV6r93pl7WKq7I5tqP6slKwZ0kaIvKxvdDZUAFuIQeQ9HvgMUCD/ZUAY0gPnhBqtR3SojyHWYch5IIsU5C+b7r3pVr/RyJvtTuvtEd0yD3jpdft6t0rUM+rNi8pMGUgddCClWQY1eOGJTNRC6N+3VDap7aGr5+WHJmFYqjEHD5x61+MmAdKzhDnocjHnWWTUQUk9PnjzlXLnysdHwxg2i7TumHlGohMcaofjAz51Qd3xOnjwlrlz5OJQ2exJkoUDVdIS08LOm1B3kwhVbDAwMhKZP2DdxgrrZUs6Tfuft7psxUV0jWk6b0Exm4YKhdAq5nM10uOOGkfMbd7SlBj2T6+zsVOr3doVoSdfevfUilUqxxFvuhuAnJnKVU5SXp8UGQuqoydaIpQSiwBt81ck3NNudgLL948ePxaNHj/I/P/30k3jyySdFTU1N/mfnzp2ivr6eBbZS7ZZfVfIDoMyCUdvKUNUIFRMs1Qarljt48KBz+/ZtX/NDGXzaPOiTT/4qmpqaqlYvVV1Vy9EW9seOHaedsPOHmN4skzE4ePCguH37dlXqX5WdsjU/LJ4nRn2NoipAfsvJGzA24JNtqbZ5oFvjqoWQOul+SO7nSiyvxvTfF198UVy7do1tfuUXkCCPp3kszWFpLusXQLfmQS4aCFIP1XNXNYQkgq2rcnFWrNb5iapxisvJebgN+NwAhvy4yLT7vo6reghJHRu3x90qu3ZuC3yVja/ohnCw39Uv5ZpIGoexcCIEiTyriAWEpEJvb68zNjbm6wZBsZpuGIeHh8WpU6cq6s6kZ/QNC9Ad2vfff1+MjIxsnMHvcL/4Isf5eMhQFuPDYgMhKeReNGxr2OQeOtHvzc3Nor+/n57vVZ22NMeemPizmJ+/aRU+98WsECcxMzNTdfqVozQ2HZUCyIxoE8Li+aL8Oy2vam1tFY2Nf7C5jYbxFVf3QFrFs7T0NzE/P0/LxDYdbjvzueIjxsbGYuXLWHVWBlrOEYMAsRyQzz77G/HKK22ioSEhDhxIiYaGhooCk4C7d++e+PLLZfHVV/8Sn356TXz//X8CA694+El/j8scsPhiGEsISQS503OQIJYD0h0E2p5h166nxQsvPF9Y8VKjm7C0yz9+vL7a5uuvvxFraz8K2h6k7FBpxxPa59c5QA5F/XxARqe+SiwbWwgpGLbWmZoEtngeZHIO28fYHGKqtK2a14Oq9F+WiTWEJALd6Tt27Bh9wyCvSdhG1AlWtZSV8EX12xG24xB7CKWg8nNcANG2xTafTwJIj3SGhobgv2ra3sKGdeiN91dffVXQF2IBow1FfzmHhK+urk58+OGHoq2tDQAW5IEQJbxmewmWXTtH72wSQCz1Kx07QFjG07QYmR66LywsICsaci/howUMly9fxqL3MjoCQg+D0SqR117748YzM9y48SZSwkePXz744IOqXD3krYJ6CUCoqBXBSGsl6UXhjVvLAT9DU2xaRRRzP3KhF3Bp+8hqXLoXhNiAUFNVunnz1ltvAUba9tz5eZN6BN/bb7+Nmy6angKEmoLJ4jRnnJ2dpfcVN+6mxuWOqhs+Wo7X3/8n0d7ejjmfoZcAoaFw7sNo42Fa4Fy8yLlaoCy1uocWp9NPS8sheMinhyCgTwGLD89kMs4XX3whFhcXxdTUVNmz0w0eMnel3OjxWkbX1dVFG12Jl156SaTTafjGom8gpkUxS52Khq137vw9v+vY559/Lu7d+6e4e/eOdq0msHqBVaoR9I2HhobfiZdfflnQLnP79/8ew0ztaOkdAAj19LJWmtas0tsLDx8+FI8e/U/8+OPD/O/Ff2pqdgr5Iq0OiARga+thcfLkyZJt3r17t3j66d2ipuZXgn5/6qmn6FNl8IO1CKufCKKra8VW0mRrDoIQK1TYQqZVMSDUkounsCmEcdqnhScydmoFhHZ0DPQsgDBQedlPDgjZQ+DdAEDorVGUSwDCCEQPEEYgSD6aCAh9iBfWoYAwLKV56gGEPLpr1QoIteSKXGFAGIGQAcIIBMlHEwGhD/HCOhQQhqU0Tz2AkEd3rVoBoZZckSsMCCMQMkAYgSD5aCIg9CFeWIcCwrCU5qkHEPLorlUrINSSK3KFAWEEQgYIIxAkH00EhNuIR+8C0odTuP+8++679L0+rReA6S2Kjo4O8cYbb3A3P/+hm1QqBa+ViQSEKRLG/bUmdve6GqDzLqE8zOSl3qD7HIdv0OtqCAhdijU3NzuVttlvJW2BoWuu4vLuzYAXFhbgvYJAEKIgRGvrYYc+A22ScfyaM27Hy7f+5+dvwn/4IMy6/cfHx52enh4AGOLVgEDE0HRdcFyJ1nVw8mJgR+3QMHTNV2PvwdgLQN9qTyaT61ckQBg6hNlsViSTyVj7MNadJ8fRPqHpdBoAhobfLxVRNsxkMrHfxzT2ENKzwFQqBQhDhlAOR5eXl2P/DDH2EBa8hzkhE4S4L4EbM3nrJRIJJ5fLIRuGCCJlwkQiIXK5XOwTQewFIN/RB11aWg7h5kxIEMqh6K1bn+GDMhgK/OK6wcHB/EdAcZc0WBIlgPQR0ZGRESQBQLjZcKOjo05fX9+m/4nHFv6hLF7DOjo6Kvr6+gBgQVoIUeQx+lDL5ORfxMzMNZHN/kOsrf2AuaIPDuXa1wMHUuL48eP5n/r6evjOpen/AXIKoGkaeCMuAAAAAElFTkSuQmCC"><h6>~a</h6></button></nav>')
	:- listening.
% non-listening icon
header('<nav class="navbar mb-1"><button class="btn"><img class="img-fluid" style="width:4rem" data-value="Mic" src="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAOEAAADhCAYAAAA+s9J6AAAd3UlEQVR4Xu2dfWxdRXbAj82XISBvSBZD2G1tIHlbKeVBgBgJ13+Ag2RkQVzJfLjkgwVcsE1l3BWOQRAFRIxXbbCKbcAhJSSsFdYCm8WSq3xQapkVDguLtwHJCcQuC2HTDaRWCDKw3KnOu+/lvvf8nu/M3Lkfc++xFJHguffOnHN+c+bjzJkCoJ9AS2B8fJx98cWf4PDhw/C7370Lf/7zMfjyy2Nw/Phx+Oqrr2Bm5gQsWLAALrhgMSxcuBAWLVoMP/7xYrj66mvgkksugYsuuhDKy8sLAt3IEFeObXmYwfanAGaNvK0k5QTMAIaGXmdvvfWfMDIyApOTkxy1KwSA/ApOvSAWi0F1dTVUVVVBTU0N6Z1Dsk6L8ACI3yBlOJW0w+cnJyfZrl27YGDgNThwYCLP2xA0Jz9zIV2+PA51dX8Pt99+O8RiMbIDJ+LN8SwvgAShYsGLvG7nzpdZf/+vEh4v88cpcDy1yIQSPWR9/T/AmjV3Eow84rMpIwIgQahA4KKveOKJJ9mjjz7iA3j5apoJ5KZNm2Djxo0Eo6hik+VFASQIJQUt89imTZvYxo0b0x71wuOJ1tQCkmAUlR2ADIBQVEhzQnFRiz3R29vLGhsbAw5fdpssGHt7e6GxsZE8o+IhaOJ1RYUA6zcQhGJI8ZeemJhg9957L+zfvz/5UBA9n117TBhXrlwJW7duhXg8TjDmEJmsB0wA2Lq5gIRqZ4cSv29paWFdXV0aw5fbM7a0tEBXVxfZTJp4nAJIc0IJwOZ7ZHp6mt17bwPs2bM7RACmWmx6xVWrboSenm5YtmxZ5GFUASBBqBDC4eFhVlNTo9ncT0YAJozDw8OR3vRXBSBBKGODOZ7ZsuVp1tr6YAi9Xz4BmSBu2fI0tLY+GDmPqBJAglABhO3t7ayjoyNCAGYOT9vb26GjoyMyIKoGkCB0CGFDQwPr6+sDALdXPu1jQ/M3xf26NTQ0QF9fX+hBdANAgtABhPX19ay/v98lAOdCh9sEV1xxBfz0p38NV14Zh6Kic6Co6MxTLZid/Q5mZ7+BDz/8ED755BP44IMP0rZH0hvqBpQGrF27Fnbs2BFaEN0CkCCUhNAagqo06EzwKisr4brrroMbblgFVVXXSxv33r1vsn379sDbb78No6OjWS1WW/+wbmG4CSBBKAFhZ2cna2trU+wBTQDLysrgvvvug9raWle2AHAL5ZVXXoHnnnsOpqamXJjHGtDZ2QltbW3SnYaESlx9xG0ACUJB9eHJhzVr7lRovCZ8S5ZcnDBeL08xYFuwMzly5HPl7dm582VP2yKoRu7iXgBIEHKrAwDD0OLxuCKDNeFbsOA82L7936Gurs43zzEwMMDWr/85nDx5QmnbJiYmtA5z8wpAglAAwrKyMmYO4ZzOo0wAg7a0r3arxUgMraempnzrXARUO6eolwAShJyaUrcSakBJSQns3r07kF4CvX119U3JIarzzqa+vh76+/u1AtFrAAlCDgitcDQnRml6P12M0up0sNbO2q1TeJsfABKEHBACADOLyRqjCaBuy/dqToKc2nYJvDf0C0CC0AZCyxBlALT2/XSNsVQTE2sEvgPyE0CCcB4Ina+GmhDqfjI9MzOAfGc0Pj4eyPynfgNIEM4DYW1tLRscHHQ0DA1LnhYrP44MhChkIxGAMDg4GKhhaRAAJAjzQDg2NsYqKioc7JsZELagZmfB6uaoYGxsDCoqKgIBYlAAJAjzQLhq1Y3MPB0v0/MbgIl1DxyYCISx8a098ZVavjzOzATFcnLBU/l79uz2XS5BApAgzGF7zuaCZo8/PT0NpaWlvhsbH1r8pTD2tLS01NEIwe9ImqABSBDmsL+mpibW09Mj3dvrvhBjh6S1UCPnDZuamqCnp8eXDiqIABKEuS1Ocl/QSCRBCsJwyw4kp7+XH677t28YVAAJwixrxGDmuro6aS/o91DLKVy8z1tDdjlvODAw4GnQepABJAizrK66upqZF7SIGpehTUgaL2h25eTjaY3EFW0jIyOeDEmDDiBBmGZp8osO5hArKl4wJTL5BSzvhqQ6AEgQpkGIl3OuXn0LeUE7F5j2eyfecGjodVi9+hbXvKEuABKEaQYlvypqBGoTWoAhx0WtoAbx4bubq6Q6AUgQppmh3Ea0AYsXL4Zjx4651qM7JsXlF5SUlLCjR48KjiDcC2jQDUCCMGmgTuY3uh1RUs2k3EkTd+bROgJIECYtUn5rIrpD0RTMToakKrcqdAWQIExaktwpAe9W+VR7LxfeJxHgYICqUyY6A0gQJq1Rbn8wmMdzXADM9pVyx77U7BfqDiBBmDSv4uJiNjMzI7y4oOuJeVuqBAtYJ/BFVkkNKC4uhpmZGelFrTAASBBaxiY1nNIpiZEgV0LF5ZJhORvOhwVAgtAhhFGLkslHplwsqTyEYQKQIDTP/kmekUsYkfRQSsjV6FGYicXcmhAePHhQ6N6NsAFIRgQAk5OTLBaLJc1cbE5DEGb0DlIQTk5OQiwW4+rMwgggQWgGXifvmBAGkOSX6aEF59ViG/ZhBZCMCADGx8dZeXm58Mpo0v64enA9RpOOaykIIX7PQPnbpkIMM4AEIUHomLy0F7gCYdgBJAgJwkBDGAUACUKCMLAQRgVAgpAgDCSEUQKQICQIAwdh1AAkCAnCQEEYRQAJQoIwMBBGFUCCkCD0HcJ3x0bh6v3/AbD9KYBZ605H24oVFQKs3wAFrZu136vVvgG2yrIpQJv1TiV46nnhfcLTwIDPfrEOLty9M7IAkickT6iMQNFrxc8pYLBlIYOGiwqg4Pskvzy1CZEHTDWXPCGFrfGYPk8Zbk+IAHYtYrBuIcCZIhYYQgDJE5In5IGLtwwXhATgXHGK9EO8ytCqHM0JlanLFkICMLesCUIajqqicF4ICcD8YiYICULXISQA5xcxQUgQugohAWgvXoKQILS3Er4Sc4ajBCCf4AhCgpDPUuxLZUBIANoLjPYJkxKg1VF+Y7EpeQpCAlBMpuQJyROKWUz+0gkIzykooI14QYkShAShoMnkLc5OA4BnFwNFwghKlCAkCAVNJnfx0wBYz/kAdy0SC0VjZxRAwd3toTgNIStIgpAglLWdjOeeOx+YKIDfMYCvblkHF/3LS5G2w0g3Hq2IFmacM4gHcr97drNQMDYC+OKXAFf9ZhSuqaiMtB1GuvEEoRoARQ/kIoAvHQe4/xjAbzmS/zqvZbDfQBDScFTaQmVSUqQAbPmyAL5hjCsDt3QFNXmQICQIpUzVOYBoenxp8KUqqNFDBCFBKGyuagDEzxKEKAWCkCAUglAdgARhSvAEIUHIDaFaAAlCgjApAdqi4GNQPYAEIUFIEPLRh2nUtjzMZLchzFXQfAMumhPSnJA2621BdA9A8oTkCckT+gwgQUgQEoTzQuiuB0x9moajNByl4WhOEGUBxFjQ1uPzzQGzP0cQEoQE4RwIZQCEokJ4/jMDmr4C+AEKbYe5VgGCkCAkCDOAkQUQb0c6vXUz/JB4G0Eo0AslitJmPW3WJwzBCYDJ68lsM3DPNU7yhAQheUJVACY4NiEjT0ieUFACUY+YUeABUxInCAVtj7YoaItCxRA03ewIQoJQTgJR9YQKPSB5QjnTO/UULcxEcGHGBQBpTugARIIwYhC6BCBBSBDKSyBKw1EXASQI5U2Q9gmjAqHLABKEBKG8BKIAoQcAEoTyJkieMOwQegQgQUgQyksgzBB6CCBBKG+C5AnDCqHHABKEBKG8BMIIoQ8AEoTyJkieMGwQ+gQgQUgQyksgTBD6CCBBKG+C5AnDAqHPABKEBKG8BMIAYQAAJAjlTZA8oe4QBgRAgpAglJeAzhAGCECCUN4EyRPqCmHAACQICUJ5CegIYQABJAjlTZA8oW4QBhRAgpAglJeAThAGGECCUN4EyRPqAmHAASQICUJ5CegAoQYAEoTyJkieMOgQagIgQUgQyksgyBBqBCBBKG+C5AmDCqFmABKEBKG8BIIIoYYAEoTyJkieMGgQagogQUgQyksgSBBqDCBBKG+C5AmDAqHmABKEBKG8BIIAYQgAJAjlTZA8od8QhgRAgpAglJeAnxCGCECCUN4EyRP6BWHIACQICUJ5CfgBYQgBJAjlTZA84cTEBIvH4wBQKCBGI1VW+H7HkAIoDeHExATE43FhOQooK/BFI9141I4FIf5LGEQh+YUYQAkIzY6MIATyhAcPHmTLli1L9pbuQRhyAJMQCssPDh48CMuWLRPqzALv2gQrGOnGo6ymp6dZaWmpFISTk5MQi8VsZRh2AK2OTBzC6elpKC0ttZWhoF1rVTzSjU/TFDP/LmZEe/e+CVVV188rw7ADiFLbu/dNVlV1vbD8kvKPvA1GXgBJQ5CCcOfOl2HNmjvzyjAKAKL8du58ma1ZcydBKOl/tYZwbGyMTU1Nw8zM/0FJSQnOLaRW2iorK9no6KiwETU0NEBfX19OGUYFQLS7hoYG1tfXJyy/yspKGB0dFbZBXEzDueTRo0ehuPhHsGzZUigvLxd+jyQzyh/TsuI4/GlpeRAOHJiYIxBU7DPPPCMEY0tLC+vq6hI2orKyMpiampojwygBiAooKytjU1NTwvJraWmBrq4ubhtE+B544AEEd47eY7EYbNu2DSoqKrjfp5wmyRdqV+HOzk7W1taW1tz0edyp/TscIs07VEyXl5PhVPYSe9QAdLLFI6ejlOZy672zsxPa2tq0smutKjswMMDq6uo4VjLF9qBwWFtRUcHx3uyuzoDu7m5obm5OyDFqAGKbu7u7WXNzs7AXxGfHxsa4PBc/6KbeBwYGoK6uThvb1qaiKNwFC85jJ0+e4FS4AatW3Qh79uzmamNxcTGbmZnhfHcKRgNWrFgB77//fkEUAUQprFixgr3//vvCcisuLoaZmRku3axadSPbs2c35zcMEHm35AhS6WNcQlD6RcmXDQ8Ps5qaGk5F4EfEvGFtbS0bHBwUeL/5jdMA4E9tP4fFI9sBZq3hsG0ziwoB1m+AgtbN2uggu01yIX+m3Gpra2FwcNC27fxeML12BgwPD0NNTY3t+2315EEBLSqJcmhvb2cdHR3CkPT29kJjY6NtO3t7e1ljY6PQ+88pYLBlIYO7Swrg9B+Suxw8SgsBgNjMTZs2sY0bNwrJzBSPwT1nl9ELvr+9vR06Ojps9c6jLrfLaFFJFILsMjjvRF20V0cAuxYxWLcQ4EwRKYYEwKRhSuyvmhDyxoxaC3FigRTzbR+5DZXo+0XMR/TdSss3NTWxnp4ewV7XwN4aNm7cyNVO3vkNAQjwxBNPskcffURQHyaAqXk0j4HIeVsDmpqaoKenh0vvPPVws4wWlXQyHBXZi+LpdQnAU+Yo7QV5Ryf4Jdk9XBqOutBtyPW8/AsAWGW7ExUEoKlYOTBML4g/IkHbsgtmIiMgF8xV6JXaeMItW55mra0PCg5/DKiuroaRkRHuduZbDicATbuyMhHgv0TmaSaEIttG+ER1dTUbGRkR/JYBW7Y8Da2tD3LrXYgaxYW1qCS2WXaVbPnyOIa3cbcz14kAAtCyulgsxvAIlziAJoQ8J0/SbXz58jgzwxNFgDfQXrhWxRXzJPU6buOUervCh6xoGTFlJKsg2s5T8x0C0FKi/DDUGoqC+EFyibmnoVXUjKhxKsRK7FVym7am8kXmIPhEKpb0nIICqW2I7xjAVEUN/OzFYW3ka6cNK75WbhialCt3PG9y7pg8cC3e8fJugdi124vf62YkUr2i6BAIBX/ZkiXsF7NH4K5FYvuACOCLXwI0fQXw2/FxrY/YpAzQ2TzQ7AjznTiZz8itmF5xCCU8rhe85fyGVhDKHpkRidZPSenQPavZX/3XkNBGPAL40nGAli8L4Btm9hfjmoMoPwJJtzcDhoZeh9WrbxGyN9nTLUuWXAxHjnwu9C3fCNSpt0AhiQXypsRqQH19PfT393MrRSYYOxPA1KfE4lf9NIRc31YFoOzh3fr6etbf3y+8KCO6Auu33LkN0++K4vflFgYQBBzOGFxtVQeg1Qng32SGxH7K3FolTtVCZEiY2XYH8zOp6YdIgIafMk59m8swg1DR5MReOpcJz7BQPYCZxigzJPND9kNDr7PVq29JfloGPqvdspvm8vNQ/uBwP2Sb65taQehEMU888SQ8+ugjedvrHoCZIAY9sNgKlMd6OwPQybk+2QgpHefhWkGIAl6y5GJ25MjnggZi4HN5J+vuA5i5SLFy5cpEHpwgJSfCDg7zt+zfv19Qtrn6dudz4ZKSEoaJnMQ6gvn1HBTPl10P7SCUO9KEzTZyrlR6C2CmV1y7di3s2LHDdx2o835W+2RWpFNPy13SY+o46CMN7Yej2AC5yBlTQdlDUn8AzAQRe/rOzg5fkhPNnzRL1m+Ir0Znf0nu+JKpY93yy2Ctfe+FRVXtJG198luJNssACGcVwhtFS6D23c/gB0fzpczhaepfuKp3xx13uDpMRS8zNDSEp87TKuFk7pfZFkXbA1KrolgT0egoUftzo7x2EKIQVq5cyeTmLgb0/VsX3PuX/wXY/pR0ThgrKZTTxYvcMOK+2u233w7XXXedUP7UfAaC+31vv/027Nq1Kytnpzr48NuLFy+GY8eOObIpuQ160wviXHv//v2Ovu8GZHbv1K7C2CC5ExUAp4EBPecD/OPFhQDfOkvKJLdwYKcO05iyf9C7VFXdALHYz+Ciiy5MZBvPdYkKjhJwMePTTz+Fw4cPw969+zDbXI6PqoLPqi/W6ejRoyrsScILmvXQ6eREulJUCI3HspSWkRmSqj4NgXXAy0XF0ySKimL+ziK5DWDzUpXQZX/KTDGIG/JOb1dyMt/XdSiK9dYSQqy4yIlr1QCmm6H80FgUxlR5Hg/uJnSZQ2iVQ0D5s4piGRRkJe/Wc9pCaEV1zG9wbgKYUor3ILplDiLvVTsH48nvk792cgHiIq11s6y2EKJQ7DJyewFgSjnq99rcVLuTd5ueWGU2M7vcPvPX1kA7gJMnT2hry9pWHBUzX94ZLwFMGYm1YIT/x6shoROgRJ61hsGqF0BEphZza5x5H4hIi4JSVmsIUYi57pDwA8CUQicnJ9nNN98MZh6WsMBoAojXj73xxhtK75iXXek2ZavfvRO5wNcewuy5hJ8ApgvYivrQHUQTQNnTEPN5G+ywEGz5zsoAkRymQfF82fXQHsJkgxJ7S7I5Yb7HCJq7/hnOav9XpfJAI2tufiBtr06nIaoJH6aMxAtUY7GYUtkkPaujzG1J3Suvl9ewat8AFBh6w4fb2uDZxSB8N0QqJ8xbf3cz7Hr9N67IA/e/HnroIbzV10Gv74VpWPM+zAnzy1/+0rV7/qx5oOxIIRxeEFvvitF5YS7Z39j1NwtZ7V+OO8oJ43bqdLze7fHHH08eF0q1IAje0YIP9/0ee+wxV68Vs27YkpWBAaL5ZP2wSd5vhgJCDMY2tnVA4ff815PlywnjxRwDYcTLbczM0uk/XgKZuemPw07cdnD7Tr/Mkxsy7TXrrVu6kPmA1B5CmdMQuQFMicnbGERcHXzhhRfwtl8PgMwED29HuueeezzLVO1sJdTST1DOYfJ6OrtyWkOoHkAUl2moXnjEdOXgIs6+ffsS97ibGcZ4f1LehCecDRKZ5yoqKuCGGzAgXP1iS75aO4uIsQDEv+l4XCmUntAdANNF5e8dd3j86ODBg/DRRx8l9hw//vjjrLnk/JCil1u+fDlceumlEI9fAWVlpUqORfF2Denl1EQTmZ2MLsmyROSkpSd0H0Cr5xW91UlE+DJl8fTGt99+C7Ozs4k/33//PZxxxhlQVFSU+HPWWWc5Ps0gU698z1i3KmEJmTmgpQvdUhnyylE7CL0D0FI+Jon69a9fwWGcdvLiNQTV5TCF/a233obJtZKvdgagyO2+qtvi9vu0MirvAcyci3R3d0Nzc7NWMnPbgHK9X10MrTXPDds8MF1u2hiUDIDGGQWw9QuWuJzFeU4Y0yAuu+wyePXVV32bX/kBFe83cR576623KoybNWWuY/ImXplhOS0glAEQigoB1m+AradfAA3/1KJgSJTpFcM6PxExnvSy1hUFTud+mXJWfWJDtn1uPhd4CJ0AWNC6OdE+Ncvj6WqwhkluR9m4qXwV73Ye/ZKvFuEJS7OTc6AhVAFgSgBNTU2JKBVnK3TZ4rRgxFMG69atC9TKpJ3yZX+PK7TPP/+8S2kTsVb+bg/JykX2ucBCqBLAlHAyg4ZVDZtMo0n9YLpCTCdfV1cXWNnKGgsGovf1bc3K4OZk1TNz6JmmJxgcHAyd/PLJPZANdQPAuR5RJYS5jQnDqzBd4cqV13ganSILWfZzGMWzf/+7Ceh27NiR9WsV8M2Vm8q0Gark4PZ7AgehmwCmhOk8iJhHLZlhZJgY96abboKlS2Nw5ZVxWLp0aaDAROAOHToEv//9BPzxj/8Dr732GibydRG89Ff7EyrIo0UvygQKQi8ATAnVyvTshkfMP3fM/g3m7Fyw4Fz4yU8uTka8FLmu92+/NaNtPvvsczh58utk7tR8n1Xp8XJ9wwTQyQUyrgvM5Q8EBkIvAUzJVN1lmDJa4gu4lnmz/DNuA5e7cwpjPKiIDgIBoR8ApoSEK324wWzebeGFVxRRT1jLmh2QysTBOkvKdwj9BDBdceFJzBR0c3QvcVTQW56vfr5CGBQAU8LBE+933303XmxCXlG5RZvw4cUx27Ztc/0Ev/Lqu/hC3yAMGoDpMlYfguWiBrV4tQkghfrlVpYvEAYZwJSYMBgZN91HR0fJK0qDbsKHAQzPPPMMBb3nkaPnEOoAYLqsMErk/vvvT9sz83oFUZoAHx804cPtF8yfE8boIZXC9RRC3QDMhhGvmM5MyERAWjKytlzwAC4GthN8fKh6BqHOAKaLEhdvMC8nwYhSmZu9DfOqup02kc+09SnlCYRhATBdrThnHBoaSlzRbK2mYokoeEcLPgzHw7nz6tWrac4nyb3rEIYRwGxZ7937JtuxY3uOIOewQDk3ugeD09euXQ9VVde7bkOStq3NY64KMAoAZmt6fHycvffeexz5Q9FjonEHxXPOH0aXyld61VVXQXl5uat2ow09iirqmjCjCGAuneCw9Q9/+O9E1rF33nkHDh06DAcOTEioTwZW8fhUvONh6dJL4NprrwXMMnf55X9Lw0wJbYk84gqEBKC9CjBmdWZmBk6cOAGzs9/B11+fSPw9+6eo6CywDtKKgGgkzjLisDHXz3nnnQfnnnseFBWdCfj3s88+W+nln/YSoBIpCSiHkABUb1xyqTkMilBRrwpX3qgUQgLQFR3hbUkS+XGilafFHcl781ZlEBKA7imMIHRPtkF4sxIICUB3VUkQuitfv9/uGEIC0H0VEoTuy9jPLziCkAD0RnUEoTdy9usr0hASgN6pjCD0TtZ+fEkKQgLQW1URhN7K2+uvCUNIAHqtIqAtCu9F7ukXhSAkAD3VzamPkSf0R+5efZUbQgLQK5XM/Q5B6J/svfgyF4QEoBeqyP8NgtBf+bv9dVsICUC3VWD/foLQXkY6l5gXQgIwGKolCIOhB7dqkRdCAhAAzwLixSl+/zz11FN4X5/gAWADamtrYcOGDX5XP3HRTTwetx11+V5RnyqQUzBRBjDztiaftJLzsyJnCVMvED/U63aLo3AHvagM50AYZQArKytZ8JL9BikFhqh5ZZe3kgGPjo6SZ0yKJ0MQUQZw1aobGd5IG5ycL04NPsjPm6f+9+zZTSACwCkhRBnA3t5e1tjYSAB6yq2RSBfZ2NgYeRATAogygEm7Y+Z/ZeZdnlpuiD52ar5KEEYdQLyrPRaLJY2bIPSOchPCyclJiMVikQaxgF1eyGBWYBWtqBBg/QYoaN0cCsFhntDy8nLygt7Rl/YlA8bHxyOfx7SALYPkUIxDCyEDEFuMe4HxeJwg5FC/2iJmxz8xMRH5PUR+CEMIYJpR0ZxQLWEcb6M5YUpIfBCGG0CckzCcm9DCDAc7yooYKHecE4ZiWuNELPYQhhxAFB5e6FJVdT0tzjixJKFnTS+4d++bdKEM7hPOOyeMAIAp22lvb2d4CShtVQjRJFHYBBAvEe3o6Ii8F0RZ5IcwQgCmLKm7u5s1NzdnGRZtW0iQlvVI5up7d3c3NDc3E4BJKeWGMIIApqwGL2rZufNXMDj4KkxOHoKTJ/GSFgJRHkQz9nXFiivgtttuS/wpLS0lANME+v+77VLnXlWgOAAAAABJRU5ErkJggg=="><h6>~a</h6></button></nav>')
	:- not(listening).
% Main body of the page: A container class.
main('<main class="w-100 container">~a</main>').
% Footer which shows STT (received from DialogFlow).
footer('<footer class="fixed-bottom"><p class="lead mb-0 bg-light text-center" style="min-height:0rem">~a</p></footer>').

% card text
txtHtml('<div class="card-body"><p class="card-text">~a</p></div>').
txtHtml(Txt, Html) :- txtHtml(Template), applyTemplate(Template, Txt, Html).