%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Ingredient hierarchies Database							%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
* This database will be used for the ingredient type selection feature and contains only one 
* predicate:
* typeIngredient(Ingredient, IngredientType). 
* a predicate containing the specific ingredient found in recipes together with it's type.
* @param Ingredient The ingredient that can be found in recipes from the database.
* @param IngredientType The type of ingredient.
*
**/

% pork
typeIngredient('bacon', 'pork'). 
typeIngredient('pork shoulder', 'pork'). 
typeIngredient('pepperoni slices', 'pork').
typeIngredient('pork', 'pork'). 
typeIngredient('salami', 'pork'). 
typeIngredient('streaky bacon', 'pork'). 
typeIngredient('pork steaks', 'pork'). 
typeIngredient('pork ribs', 'pork'). 
typeIngredient('pork belly', 'pork'). 
typeIngredient('back bacon rashers', 'pork'). 
typeIngredient('bacon lardons', 'pork'). 
typeIngredient('pork sausages', 'pork'). 
typeIngredient('minced pork', 'pork'). 
typeIngredient('pork chops', 'pork'). 
typeIngredient('pork mince', 'pork'). 
typeIngredient('pork belly strips', 'pork'). 
typeIngredient('pork loin steaks', 'pork'). 
typeIngredient('pork belly slices', 'pork'). 
typeIngredient('mortadella', 'pork'). 

% beef
typeIngredient('stewing beef', 'beef'). 
typeIngredient('beef shank', 'beef'). 
typeIngredient('beef tomatoes', 'beef'). 
typeIngredient('beef short ribs', 'beef'). 
typeIngredient('beef', 'beef'). 
typeIngredient('lean minced beef', 'beef'). 
typeIngredient('beef stock', 'beef'). 
typeIngredient('beef brisket', 'beef'). 
typeIngredient('beef steak', 'beef'). 
typeIngredient('beef mince', 'beef'). 


% lamb 
typeIngredient('lamb chops', 'lamb'). 
typeIngredient('lamb mince', 'lamb'). 
typeIngredient('lamb shoulder', 'lamb'). 
typeIngredient('lamb neck', 'lamb'). 
typeIngredient('lamb', 'lamb'). 

% chicken
typeIngredient('chicken pate', 'chicken'). 
typeIngredient('chicken tikka', 'chicken'). 
typeIngredient('chicken stock', 'chicken'). 
typeIngredient('chicken gyozas', 'chicken'). 
typeIngredient('chicken', 'chicken'). 
typeIngredient('chicken breasts', 'chicken'). 
typeIngredient('chicken skin', 'chicken'). 
typeIngredient('chicken breast', 'chicken'). 
typeIngredient('chicken carcass', 'chicken'). 
typeIngredient('chicken wings', 'chicken'). 
typeIngredient('chicken legs', 'chicken'). 
typeIngredient('chicken thighs', 'chicken'). 
typeIngredient('chicken leg with thigh', 'chicken'). 
typeIngredient('chicken thigh', 'chicken'). 
typeIngredient('chicken drumsticks', 'chicken'). 

% turkey
typeIngredient('turkey leftovers', 'turkey'). 
typeIngredient('turkey', 'turkey'). 
typeIngredient('turkey meat', 'turkey'). 

% duck
typeIngredient('duck fat', 'duck'). 
typeIngredient('duck breasts', 'duck'). 
typeIngredient('duck drumsticks', 'duck'). 

% poultry
typeIngredient('chicken drumsticks', 'poultry'). 
typeIngredient('duck breasts', 'poultry'). 
typeIngredient('chicken wings', 'poultry'). 
typeIngredient('chicken stock', 'poultry'). 
typeIngredient('duck drumsticks', 'poultry'). 
typeIngredient('chicken legs', 'poultry'). 
typeIngredient('chicken thighs', 'poultry'). 
typeIngredient('chicken carcass', 'poultry'). 
typeIngredient('chicken leg with thigh', 'poultry'). 
typeIngredient('chicken gyozas', 'poultry'). 
typeIngredient('chicken skin', 'poultry'). 
typeIngredient('chicken breast', 'poultry'). 
typeIngredient('chicken pate', 'poultry'). 
typeIngredient('turkey leftovers', 'poultry'). 
typeIngredient('chicken', 'poultry'). 
typeIngredient('duck fat', 'poultry'). 
typeIngredient('chicken breasts', 'poultry'). 
typeIngredient('chicken thigh', 'poultry'). 
typeIngredient('turkey', 'poultry'). 
typeIngredient('turkey meat', 'poultry'). 
typeIngredient('chicken tikka', 'poultry'). 

% meat
typeIngredient('chicken drumsticks', 'meat').
typeIngredient('duck breasts', 'meat').
typeIngredient('crab meat', 'meat').
typeIngredient('bacon', 'meat').
typeIngredient('chicken wings', 'meat'). 
typeIngredient('chicken stock', 'meat'). 
typeIngredient('duck drumsticks', 'meat'). 
typeIngredient('chicken legs', 'meat'). 
typeIngredient('pork shoulder', 'meat'). 
typeIngredient('chicken thighs', 'meat'). 
typeIngredient('lean minced beef', 'meat'). 
typeIngredient('beef brisket', 'meat'). 
typeIngredient('chicken carcass', 'meat'). 
typeIngredient('pepperoni slices', 'meat'). 
typeIngredient('pork', 'meat'). 
typeIngredient('chicken leg with thigh', 'meat'). 
typeIngredient('minced meat', 'meat'). 
typeIngredient('chicken gyozas', 'meat'). 
typeIngredient('chicken skin', 'meat'). 
typeIngredient('salami', 'meat'). 
typeIngredient('dark meat', 'meat'). 
typeIngredient('chicken breast', 'meat'). 
typeIngredient('stewing beef', 'meat'). 
typeIngredient('beef stock', 'meat'). 
typeIngredient('streaky bacon', 'meat'). 
typeIngredient('chicken pate', 'meat'). 
typeIngredient('beef shank', 'meat'). 
typeIngredient('pork steaks', 'meat'). 
typeIngredient('pork belly slices', 'meat'). 
typeIngredient('turkey leftovers', 'meat'). 
typeIngredient('pork ribs', 'meat'). 
typeIngredient('chicken', 'meat'). 
typeIngredient('beef steak', 'meat'). 
typeIngredient('duck fat', 'meat'). 
typeIngredient('beef', 'meat'). 
typeIngredient('pork belly', 'meat'). 
typeIngredient('beef mince', 'meat'). 
typeIngredient('back bacon rashers', 'meat'). 
typeIngredient('bacon lardons', 'meat'). 
typeIngredient('pork sausages', 'meat'). 
typeIngredient('minced pork', 'meat'). 
typeIngredient('pork chops', 'meat'). 
typeIngredient('chicken breasts', 'meat'). 
typeIngredient('pork mince', 'meat'). 
typeIngredient('beef tomatoes', 'meat'). 
typeIngredient('chicken thigh', 'meat'). 
typeIngredient('beef short ribs', 'meat'). 
typeIngredient('pork belly strips', 'meat'). 
typeIngredient('pork loin steaks', 'meat'). 
typeIngredient('turkey', 'meat'). 
typeIngredient('turkey meat', 'meat'). 
typeIngredient('chicken tikka', 'meat'). 
typeIngredient('mortadella', 'meat'). 
typeIngredient('lamb chops', 'meat'). 
typeIngredient('lamb mince', 'meat'). 
typeIngredient('lamb shoulder', 'meat'). 
typeIngredient('lamb neck', 'meat'). 
typeIngredient('lamb', 'meat').

% fish
typeIngredient('haddock', 'fish'). 
typeIngredient('crab meat', 'fish'). 
typeIngredient('fish', 'fish'). 
typeIngredient('king prawns', 'fish'). 
typeIngredient('mackerel', 'fish'). 
typeIngredient('fish fillets', 'fish'). 
typeIngredient('shellfish', 'fish'). 
typeIngredient('minced meat', 'fish'). 
typeIngredient('oyster mushrooms', 'fish'). 
typeIngredient('oyster sauce', 'fish'). 
typeIngredient('dark meat', 'fish'). 
typeIngredient('king oyster mushroom', 'fish'). 
typeIngredient('anchovies', 'fish'). 
typeIngredient('squid', 'fish'). 
typeIngredient('salmon', 'fish'). 
typeIngredient('prawns', 'fish'). 
typeIngredient('fish sauce', 'fish'). 
typeIngredient('crayfish', 'fish'). 
typeIngredient('fish stock', 'fish'). 
typeIngredient('turkey meat', 'fish'). 
typeIngredient('monkfish', 'fish'). 

% non-vegetarian
typeIngredient('chicken drumsticks', 'non-vegetarian'). 
typeIngredient('haddock', 'non-vegetarian'). 
typeIngredient('duck breasts', 'non-vegetarian'). 
typeIngredient('crab meat', 'non-vegetarian'). 
typeIngredient('bacon', 'non-vegetarian'). 
typeIngredient('chicken wings', 'non-vegetarian'). 
typeIngredient('chicken stock', 'non-vegetarian'). 
typeIngredient('duck drumsticks', 'non-vegetarian'). 
typeIngredient('chicken legs', 'non-vegetarian'). 
typeIngredient('fish', 'non-vegetarian'). 
typeIngredient('king prawns', 'non-vegetarian'). 
typeIngredient('beef brisket', 'non-vegetarian'). 
typeIngredient('lean minced beef', 'non-vegetarian'). 
typeIngredient('pork shoulder', 'non-vegetarian'). 
typeIngredient('chicken thighs', 'non-vegetarian'). 
typeIngredient('chicken carcass', 'non-vegetarian'). 
typeIngredient('pepperoni slices', 'non-vegetarian'). 
typeIngredient('pork', 'non-vegetarian'). 
typeIngredient('mackerel', 'non-vegetarian'). 
typeIngredient('chicken leg with thigh', 'non-vegetarian'). 
typeIngredient('fish fillets', 'non-vegetarian'). 
typeIngredient('shellfish', 'non-vegetarian'). 
typeIngredient('minced meat', 'non-vegetarian'). 
typeIngredient('chicken gyozas', 'non-vegetarian'). 
typeIngredient('oyster mushrooms', 'non-vegetarian'). 
typeIngredient('chicken skin', 'non-vegetarian'). 
typeIngredient('chicken tikka', 'non-vegetarian'). 
typeIngredient('oyster sauce', 'non-vegetarian'). 
typeIngredient('salami', 'non-vegetarian'). 
typeIngredient('dark meat', 'non-vegetarian'). 
typeIngredient('chicken breast', 'non-vegetarian'). 
typeIngredient('stewing beef', 'non-vegetarian'). 
typeIngredient('beef stock', 'non-vegetarian'). 
typeIngredient('streaky bacon', 'non-vegetarian'). 
typeIngredient('chicken pate', 'non-vegetarian'). 
typeIngredient('king oyster mushroom', 'non-vegetarian'). 
typeIngredient('anchovies', 'non-vegetarian'). 
typeIngredient('squid', 'non-vegetarian'). 
typeIngredient('pork steaks', 'non-vegetarian'). 
typeIngredient('beef steak', 'non-vegetarian'). 
typeIngredient('turkey leftovers', 'non-vegetarian'). 
typeIngredient('pork ribs', 'non-vegetarian'). 
typeIngredient('chicken', 'non-vegetarian'). 
typeIngredient('salmon', 'non-vegetarian'). 
typeIngredient('duck fat', 'non-vegetarian'). 
typeIngredient('beef', 'non-vegetarian'). 
typeIngredient('pork belly', 'non-vegetarian'). 
typeIngredient('beef mince', 'non-vegetarian'). 
typeIngredient('back bacon rashers', 'non-vegetarian'). 
typeIngredient('bacon lardons', 'non-vegetarian'). 
typeIngredient('pork sausages', 'non-vegetarian'). 
typeIngredient('minced pork', 'non-vegetarian'). 
typeIngredient('pork chops', 'non-vegetarian'). 
typeIngredient('chicken breasts', 'non-vegetarian'). 
typeIngredient('pork mince', 'non-vegetarian'). 
typeIngredient('beef tomatoes', 'non-vegetarian'). 
typeIngredient('prawns', 'non-vegetarian'). 
typeIngredient('fish sauce', 'non-vegetarian'). 
typeIngredient('chicken thigh', 'non-vegetarian'). 
typeIngredient('beef short ribs', 'non-vegetarian'). 
typeIngredient('pork belly strips', 'non-vegetarian'). 
typeIngredient('pork loin steaks', 'non-vegetarian'). 
typeIngredient('crayfish', 'non-vegetarian'). 
typeIngredient('fish stock', 'non-vegetarian'). 
typeIngredient('turkey', 'non-vegetarian'). 
typeIngredient('turkey meat', 'non-vegetarian'). 
typeIngredient('mortadella', 'non-vegetarian'). 
typeIngredient('pork belly slices', 'non-vegetarian'). 
typeIngredient('monkfish', 'non-vegetarian'). 
typeIngredient('beef shank', 'non-vegetarian'). 
typeIngredient('lamb chops', 'non-vegetarian'). 
typeIngredient('lamb mince', 'non-vegetarian'). 
typeIngredient('lamb shoulder', 'non-vegetarian'). 
typeIngredient('lamb neck', 'non-vegetarian'). 
typeIngredient('lamb', 'non-vegetarian'). 

% egg
typeIngredient('egg noodles', 'egg'). 
typeIngredient('eggs', 'egg'). 
typeIngredient('egg white', 'egg'). 
typeIngredient('egg', 'egg').
typeIngredient('egg whites', 'egg'). 
typeIngredient('egg yolks', 'egg'). 

% cheese
typeIngredient('cheddar', 'cheese'). 
typeIngredient('manchego cheese', 'cheese'). 
typeIngredient('cheese', 'cheese'). 
typeIngredient('pecorino', 'cheese'). 
typeIngredient('blue cheese', 'cheese'). 
typeIngredient('cottage cheese', 'cheese'). 
typeIngredient('parmesan', 'cheese'). 
typeIngredient('cheese slices', 'cheese'). 
typeIngredient('ricotta', 'cheese'). 
typeIngredient('camembert', 'cheese'). 
typeIngredient('brie', 'cheese'). 
typeIngredient('feta cheese', 'cheese'). 
typeIngredient('manchego', 'cheese'). 
typeIngredient('provolone cheese', 'cheese'). 
typeIngredient('saag', 'cheese'). 
typeIngredient('mozzarella', 'cheese'). 
typeIngredient('halloumi cheese', 'cheese'). 
typeIngredient('mascarpone', 'cheese'). 
typeIngredient('red leicester', 'cheese'). 
typeIngredient('burrata', 'cheese'). 
typeIngredient('pecorino cheese', 'cheese'). 
typeIngredient('parmesan cheese', 'cheese'). 
typeIngredient('ricotta cheese', 'cheese'). 
typeIngredient('cheddar cheese', 'cheese'). 
typeIngredient('mozzarella cheese', 'cheese'). 
typeIngredient('goat cheese', 'cheese'). 
typeIngredient('cream cheese', 'cheese'). 
typeIngredient('edam cheese slices', 'cheese'). 
typeIngredient('feta', 'cheese'). 
typeIngredient('halloumi', 'cheese').
typeIngredient('taleggio', 'cheese'). 

% dairy
typeIngredient('cheddar', 'dairy'). 
typeIngredient('manchego cheese', 'dairy'). 
typeIngredient('pecorino', 'dairy'). 
typeIngredient('cheese', 'dairy'). 
typeIngredient('blue cheese', 'dairy'). 
typeIngredient('cottage cheese', 'dairy'). 
typeIngredient('vanilla buttercream', 'dairy'). 
typeIngredient('parmesan', 'dairy'). 
typeIngredient('butter', 'dairy'). 
typeIngredient('cheese slices', 'dairy'). 
typeIngredient('ricotta', 'dairy'). 
typeIngredient('camembert', 'dairy'). 
typeIngredient('milk yoghurt', 'dairy'). 
typeIngredient('melted butter', 'dairy'). 
typeIngredient('caramel ice cream', 'dairy'). 
typeIngredient('yogurt', 'dairy'). 
typeIngredient('brie', 'dairy'). 
typeIngredient('milk', 'dairy'). 
typeIngredient('butter beans', 'dairy'). 
typeIngredient('feta cheese', 'dairy'). 
typeIngredient('manchego', 'dairy'). 
typeIngredient('horseradish cream', 'dairy'). 
typeIngredient('single cream', 'dairy'). 
typeIngredient('vanilla ice cream', 'dairy'). 
typeIngredient('provolone cheese', 'dairy'). 
typeIngredient('saag', 'dairy'). 
typeIngredient('mozzarella', 'dairy'). 
typeIngredient('condensed milk', 'dairy'). 
typeIngredient('halloumi cheese', 'dairy'). 
typeIngredient('mascarpone', 'dairy'). 
typeIngredient('yoghurt', 'dairy'). 
typeIngredient('red leicester', 'dairy'). 
%typeIngredient('almond butter', 'dairy'). 
typeIngredient('burrata', 'dairy'). 
%typeIngredient('butternut squash', 'dairy'). 
typeIngredient('pecorino cheese', 'dairy'). 
typeIngredient('skyr yogurt', 'dairy'). 
typeIngredient('parmesan cheese', 'dairy'). 
typeIngredient('ricotta cheese', 'dairy'). 
typeIngredient('buttermilk', 'dairy'). 
typeIngredient('cheddar cheese', 'dairy'). 
typeIngredient('mozzarella cheese', 'dairy'). 
typeIngredient('tzatziki', 'dairy'). 
typeIngredient('cream cheese', 'dairy'). 
typeIngredient('double cream', 'dairy'). 
typeIngredient('edam cheese slices', 'dairy'). 
typeIngredient('feta', 'dairy'). 
typeIngredient('halloumi', 'dairy'). 
typeIngredient('sour cream', 'dairy'). 

% non-vegan
typeIngredient('manchego cheese', 'non-vegan'). 
typeIngredient('fish', 'non-vegan'). 
typeIngredient('milk yoghurt', 'non-vegan'). 
typeIngredient('camembert', 'non-vegan'). 
typeIngredient('melted butter', 'non-vegan'). 
typeIngredient('beef brisket', 'non-vegan'). 
typeIngredient('chicken thighs', 'non-vegan'). 
typeIngredient('caramel ice cream', 'non-vegan'). 
typeIngredient('chicken leg with thigh', 'non-vegan'). 
typeIngredient('shellfish', 'non-vegan'). 
typeIngredient('feta cheese', 'non-vegan'). 
typeIngredient('oyster sauce', 'non-vegan'). 
typeIngredient('salami', 'non-vegan'). 
typeIngredient('condensed milk', 'non-vegan'). 
typeIngredient('chicken pate', 'non-vegan'). 
typeIngredient('squid', 'non-vegan'). 
typeIngredient('pork steaks', 'non-vegan'). 
typeIngredient('turkey leftovers', 'non-vegan'). 
typeIngredient('chicken', 'non-vegan'). 
typeIngredient('duck fat', 'non-vegan'). 
typeIngredient('parmesan cheese', 'non-vegan'). 
typeIngredient('ricotta cheese', 'non-vegan'). 
typeIngredient('bacon lardons', 'non-vegan'). 
typeIngredient('cheddar cheese', 'non-vegan'). 
typeIngredient('pork chops', 'non-vegan'). 
typeIngredient('chicken breasts', 'non-vegan'). 
typeIngredient('pork mince', 'non-vegan'). 
typeIngredient('cream cheese', 'non-vegan'). 
typeIngredient('chicken thigh', 'non-vegan'). 
typeIngredient('edam cheese slices', 'non-vegan'). 
typeIngredient('turkey', 'non-vegan'). 
typeIngredient('halloumi', 'non-vegan'). 
typeIngredient('chicken drumsticks', 'non-vegan'). 
typeIngredient('vanilla buttercream', 'non-vegan'). 
typeIngredient('bacon', 'non-vegan'). 
typeIngredient('cheese slices', 'non-vegan'). 
typeIngredient('pork shoulder', 'non-vegan'). 
typeIngredient('honey', 'non-vegan'). 
typeIngredient('fish fillets', 'non-vegan'). 
typeIngredient('egg noodles', 'non-vegan'). 
typeIngredient('brie', 'non-vegan'). 
typeIngredient('horseradish cream', 'non-vegan'). 
typeIngredient('chicken skin', 'non-vegan'). 
typeIngredient('provolone cheese', 'non-vegan'). 
typeIngredient('dark meat', 'non-vegan'). 
typeIngredient('chicken breast', 'non-vegan'). 
typeIngredient('stewing beef', 'non-vegan'). 
typeIngredient('king oyster mushroom', 'non-vegan'). 
typeIngredient('anchovies', 'non-vegan'). 
typeIngredient('red leicester', 'non-vegan'). 
typeIngredient('pork ribs', 'non-vegan'). 
%typeIngredient('almond butter', 'non-vegan'). 
typeIngredient('beef', 'non-vegan'). 
typeIngredient('pork belly', 'non-vegan'). 
typeIngredient('back bacon rashers', 'non-vegan'). 
typeIngredient('taleggio', 'non-vegan'). 
typeIngredient('prawns', 'non-vegan'). 
typeIngredient('tzatziki', 'non-vegan'). 
typeIngredient('fish sauce', 'non-vegan'). 
typeIngredient('pork belly strips', 'non-vegan'). 
typeIngredient('sour cream', 'non-vegan'). 
typeIngredient('egg whites', 'non-vegan'). 
typeIngredient('monkfish', 'non-vegan'). 
typeIngredient('beef shank', 'non-vegan'). 
typeIngredient('eggs', 'non-vegan'). 
typeIngredient('cheese', 'non-vegan'). 
typeIngredient('cottage cheese', 'non-vegan'). 
typeIngredient('duck breasts', 'non-vegan'). 
typeIngredient('crab meat', 'non-vegan'). 
typeIngredient('chicken wings', 'non-vegan'). 
typeIngredient('duck drumsticks', 'non-vegan'). 
typeIngredient('chicken legs', 'non-vegan'). 
typeIngredient('egg white', 'non-vegan'). 
typeIngredient('ricotta', 'non-vegan'). 
typeIngredient('mackerel', 'non-vegan'). 
typeIngredient('pork', 'non-vegan'). 
typeIngredient('milk', 'non-vegan'). 
typeIngredient('manchego', 'non-vegan'). 
typeIngredient('single cream', 'non-vegan'). 
typeIngredient('saag', 'non-vegan'). 
typeIngredient('mozzarella', 'non-vegan'). 
typeIngredient('mascarpone', 'non-vegan'). 
typeIngredient('yoghurt', 'non-vegan'). 
typeIngredient('streaky bacon', 'non-vegan'). 
typeIngredient('burrata', 'non-vegan'). 
typeIngredient('skyr yogurt', 'non-vegan'). 
typeIngredient('buttermilk', 'non-vegan'). 
typeIngredient('beef tomatoes', 'non-vegan'). 
typeIngredient('double cream', 'non-vegan'). 
typeIngredient('honey mustard dressing', 'non-vegan'). 
typeIngredient('beef short ribs', 'non-vegan'). 
typeIngredient('pork loin steaks', 'non-vegan'). 
typeIngredient('chicken tikka', 'non-vegan'). 
typeIngredient('cheddar', 'non-vegan'). 
typeIngredient('pecorino', 'non-vegan'). 
typeIngredient('blue cheese', 'non-vegan'). 
typeIngredient('haddock', 'non-vegan'). 
typeIngredient('parmesan', 'non-vegan'). 
typeIngredient('butter', 'non-vegan'). 
typeIngredient('chicken stock', 'non-vegan'). 
typeIngredient('king prawns', 'non-vegan'). 
typeIngredient('lean minced beef', 'non-vegan'). 
typeIngredient('chicken carcass', 'non-vegan'). 
typeIngredient('pepperoni slices', 'non-vegan'). 
typeIngredient('yogurt', 'non-vegan'). 
typeIngredient('butter beans', 'non-vegan'). 
typeIngredient('minced meat', 'non-vegan'). 
typeIngredient('chicken gyozas', 'non-vegan'). 
typeIngredient('oyster mushrooms', 'non-vegan'). 
typeIngredient('vanilla ice cream', 'non-vegan'). 
typeIngredient('halloumi cheese', 'non-vegan'). 
typeIngredient('beef stock', 'non-vegan'). 
typeIngredient('egg yolks', 'non-vegan'). 
typeIngredient('beef steak', 'non-vegan'). 
%typeIngredient('butternut squash', 'non-vegan'). 
typeIngredient('salmon', 'non-vegan'). 
typeIngredient('pecorino cheese', 'non-vegan'). 
typeIngredient('beef mince', 'non-vegan'). 
typeIngredient('pork sausages', 'non-vegan'). 
typeIngredient('minced pork', 'non-vegan'). 
typeIngredient('mozzarella cheese', 'non-vegan'). 
typeIngredient('feta', 'non-vegan'). 
typeIngredient('crayfish', 'non-vegan'). 
typeIngredient('fish stock', 'non-vegan'). 
typeIngredient('turkey meat', 'non-vegan'). 
typeIngredient('egg', 'non-vegan'). 
typeIngredient('pork belly slices', 'non-vegan'). 
typeIngredient('mortadella', 'non-vegan'). 
typeIngredient('lamb chops', 'non-vegan'). 
typeIngredient('lamb mince', 'non-vegan'). 
typeIngredient('lamb shoulder', 'non-vegan'). 
typeIngredient('lamb neck', 'non-vegan'). 
typeIngredient('lamb', 'non-vegan').

% pasta
typeIngredient('orzo', 'pasta'). 
typeIngredient('bucatini', 'pasta'). 
typeIngredient('pasta', 'pasta'). 
typeIngredient('spaghetti', 'pasta'). 
typeIngredient('tortellini', 'pasta'). 
typeIngredient('conchiglie', 'pasta'). 
typeIngredient('mafaldine', 'pasta'). 
typeIngredient('conchiglioni', 'pasta'). 
typeIngredient('linguine', 'pasta'). 
typeIngredient('rice vermicelli', 'pasta'). 
typeIngredient('lasagna sheets', 'pasta'). 
typeIngredient('casarecce', 'pasta'). 
typeIngredient('cannelloni', 'pasta'). 
typeIngredient('tagliatelle', 'pasta'). 
typeIngredient('lasagne sheets', 'pasta'). 
typeIngredient('rigatoni', 'pasta'). 
typeIngredient('vermicelli noodles', 'pasta'). 
typeIngredient('fettuccine', 'pasta'). 
typeIngredient('paccheri', 'pasta'). 
typeIngredient('penne pasta', 'pasta'). 
typeIngredient('gnocchi', 'pasta'). 
typeIngredient('penne', 'pasta'). 
typeIngredient('pappardelle', 'pasta'). 
typeIngredient('campanelle', 'pasta'). 
typeIngredient('macaroni', 'pasta').


% lactose-free recipe should not use dairy products.
typeIngredient(Ingredient, 'lactose-free') :-
	not(typeIngredient(Ingredient, 'dairy')), !.

% pescatarian: eats fish but no meat, so anything without meat goes.
typeIngredient(Ingredient, 'pescatarian') :-
	not(typeIngredient(Ingredient, 'meat')), !.

typeIngredient(Ingredient, 'vegan') :-
	not(typeIngredient(Ingredient, 'non-vegan')), !.
	
typeIngredient(Ingredient, 'vegetarian') :-
	not(typeIngredient(Ingredient, 'non-vegetarian')), !.


% Predicate to retrieve all ingredients that fall under this type, 
% or are specifically mentioned in the recipe.
hasIngredient(RecipeID, Ingr):- ((ingredient(RecipeID, Ingr)) ;
				typeIngredient(SubIngr, Ingr),
				ingredient(RecipeID, SubIngr)).
				

% Identify all filters that conflict with the (newly requested) Params. 
conflicts([], _, []).
conflicts([ Param1 = Value1 | Params ], Filters, Conflicts) :-
	conflicts(Param1, Value1, Filters, Conflicts1),
	conflicts(Params, Filters, Conflicts2),
	union(Conflicts1, Conflicts2, Conflicts).

% Identify conflicts between (new) request (ParamName = Value) and filters (that are 
% already in place).
% conflicts( +ParamName, +Value, +Filters, -Conflicts)
conflicts(_, _, [], []).
conflicts(Param1, Value1, [ Param2 = Value2 | Filters ], [ Param2 = Value2 | Conflicts ]) :-
	conflict(Param1 = Value1, Param2 = Value2),
	conflicts(Param1, Value1, Filters, Conflicts).
conflicts(Param1, Value1, [ Param2 = Value2 | Filters ], Conflicts) :-
	not(conflict(Param1 = Value1, Param2 = Value2)),
	conflicts(Param1, Value1, Filters, Conflicts).

% Define all possible conflicts
conflict(ingredient = Value, excludeingredient = Value).
conflict(excludeingredient = Value, ingredient = Value).
conflict(ingredienttype = Value, excludeingredienttype = Value).
conflict(excludeingredienttype = Value, ingredienttype = Value).

conflict(dietaryrestriction = Value1, dietaryrestriction = Value2) :-
	conflict(Value1, Value2).
conflict(dietaryrestriction = Value1, ingredient = Ingr) :-
	typeIngredient(Ingr, Value2), conflict(Value1, Value2).
conflict(ingredient = Ingr, dietaryrestriction = Value1) :-
	typeIngredient(Ingr, Value2), conflict(Value1, Value2).
	
conflict('lactose-free', 'dairy').
conflict('dairy', 'lactose-free').

conflict('pescatarian', 'meat').
conflict('meat', 'pescatarian').

conflict('vegan', 'non-vegan').
conflict('non-vegan', 'vegan').

conflict('vegatarian', 'non-vegetarian').
conflict('non-vegetarian', 'vegatarian').

% TODO: also strange to have both vegan as well as vegetarian (the first is more strict).

% NEW TLP
typeIngredient(Ingredient, 'proteins') :- typeIngredient(Ingredient, 'fish' );typeIngredient(Ingredient, 'meat');typeIngredient(Ingredient, 'egg'). 

typeIngredient(Ingredient, 'carbohydrates') :- typeIngredient(Ingredient, 'pasta' ).

typeIngredient(Ingredient, 'fats') :- typeIngredient(Ingredient, 'dairy' );typeIngredient(Ingredient, 'pork');typeIngredient(Ingredient, 'cheese').