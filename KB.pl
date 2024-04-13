% Restaurant knowledge base for CLI

% Define the attributes of every restaurant in the knowledge base.
% Each restaurant is represented by a unique predicate with its name and various attributes such as cuisine type,
% budget range, distance from the residence hall), dietary restrictions, the type of establishment (chain or local),
% the type of food (healthy or fast food), the size of the meal (full or light), the meal it caters to (breakfast, lunch, dinner, or snack).
% This section applies #rightproblem by clearly defining the problem space 
% and #sourcequality by ensuring each restaurant's attributes are accurately represented.

restaurant('Aso Rock Restaurant', african, '£10-20', 15, [], local, fast_food, full, dinner).
restaurant('Bankes Kitchen', african, '£10-20', 1.5, [], local, healthy, full, lunch).
restaurant('T4 Boba', asian, '£1-10', 0.4, [vegan], local, healthy, light, lunch).
restaurant('Honest Burgers Old Street', north_american, '£10-20', 0.1, [vegetarian], chain, fast_food, full, lunch).
restaurant('Nandos Old Street', african, '£10-20', 0.2, [], chain, fast_food, full, lunch).
restaurant('The Old Street Chinese Restaurant', asian, '£20-30', 0.2, [], local, healthy, full, dinner).
restaurant('DanyameS Kitchen The Kenny House', african, '£1-10', 11, [], local, healthy, full, lunch).
restaurant('Sea Garden and Grill', north_american, '£30-40', 7.6, [], local, healthy, full, dinner).
restaurant('Cooked by B', african, '£10-20', 7.4, [], local, healthy, full, lunch).
restaurant('Açaí Berry Wardour St.', south_american, '£10-20', 2.2, [], local, healthy, light, lunch).
restaurant('The Best Kebab', mediterranean, '£1-10', 0.4, [], local, fast_food, full, lunch).
restaurant('Hoxton Grill', north_american, '£20-30', 0.3, [], local, healthy, full, dinner).
restaurant('The Breakfast Club Spitalfields', north_american, '£10-20', 0.8, [vegetarian], chain, fast_food, full, breakfast).
restaurant('Agege Bread London Bakers', african, '£10-20', 4.5, [vegetarian], local, healthy, light, breakfast).
restaurant('Yeye Noodle & Dumpling', asian, '£10-20', 1.1, [], local, healthy, full, dinner).
restaurant('Marugame Udon Liverpool Street', asian, '£10-20', 1.1, [], chain, healthy, full, dinner).
restaurant('Kung Food (Liverpool Street)', asian, '£10-20', 1.1, [], local, healthy, full, lunch).
restaurant('Aburi Japanese Restaurant', asian, '£20-30', 0.2, [], local, healthy, full, dinner).
restaurant('Dodam Korea', asian, '£20-30', 0.1, [], local, healthy, full, dinner).
restaurant('Ngon Ngon', asian, '£10-20', 0.7, [], local, healthy, full, lunch).
restaurant('Papelón', south_american, '£1-10', 0.8, [], local, fast_food, full, lunch).
restaurant('Avila London', south_american, '£1-10', 0.8, [], local, fast_food, full, lunch).
restaurant('Lanzhou Lamian Noodle Bar', asian, '£1-10', 2.5, [], local, healthy, full, lunch).
restaurant('Wok to Walk', asian, '£1-10', 1.8, [vegetarian], chain, healthy, full, lunch).
restaurant('Gebeta Ethiopian Kitchen', african, '£10-20', 0.8, [], local, healthy, full, lunch).
restaurant('Inca London', south_american, '£20-30', 2.6, [], local, healthy, full, dinner).
restaurant('Tinseltown', north_american, '£10-20', 0.8, [vegetarian], local, fast_food, full, dinner).
restaurant('Greek Grill Point', mediterranean, '£1-10', 0.2, [vegetarian], local, healthy, full, lunch).
restaurant('Bengal Tiger', asian, '£20-30', 0.3, [vegetarian, vegan], local, healthy, full, dinner).
restaurant('Dishoom', asian, '£20-30', 0.9, [vegetarian, vegan], chain, healthy, full, lunch).
restaurant('Five Guys', north_american, '£10-20', 0.6, [], chain, fast_food, full, lunch).
restaurant('Cote', european, '£20-30', 2.1, [vegetarian], chain, healthy, full, dinner).


% Askables

% Define the askable predicates which represent the attributes that the expert system can query about.
% This approach applies #cs152-ailogic by using predicate logic to describe relationships and
% applies #rightproblem by characterizing the attributes that will inform the system's decision-making.

cuisine(Cuisine) :- member(Cuisine, [african, asian, north_american, south_american, mediterranean, european]).
budget(Budget) :- member(Budget, ['£1-10', '£10-20', '£20-30', '£30-40']).
distance(Distance) :- member(Distance, [0.5, 1, 5, 10, 15]).
dietary_restrictions(Restrictions) :- subset(Restrictions, [vegetarian, vegan, halal]).
restaurant_type(Type) :- member(Type, [chain, local]).
food_type(FoodType) :- member(FoodType, [healthy, fast_food]).
appetite(Appetite) :- member(Appetite, [full, light]).
meal_type(MealType) :- member(MealType, [breakfast, lunch, dinner]).

% User interaction

% The entry point for user interaction with the expert system.
% Implements #cs152-aicoding by invoking the AI algorithm within a Prolog environment.
% Utilizes a clear approach to querying the user (#professionalism) and a structured method (#modeling)
% to progress from a general pool of restaurants to a personalized recommendation.

recommend_restaurant :-
    write('here'),
    % Retrieve all restaurant names to form the initial pool of choices.
    % Demonstrates effective use of Prolog's findall predicate for initial data collection (#evidencebased).
    findall(Name, restaurant(Name, _, _, _, _, _, _, _, _), Restaurants),
    write(Restaurants),
    % Initiate the process of refining the restaurant list based on user responses.
    ask_questions(Restaurants, RecommendedRestaurant),
    (RecommendedRestaurant = [] ->
        % If no restaurant matches the preferences
        write('No restaurant found that matches your preferences.')
    ;
        write('Based on your preferences, we recommend:'), nl,
        % Write the list of recommended restaurants
        write(RecommendedRestaurant),
        write_restaurant_list(RecommendedRestaurant)
    ).

% Recursive predicate that filters the list of potential restaurants based on user input.
% Illustrates #aicoding by implementing recursion and conditional logic to refine user choices.
% Also demonstrates #modeling by showing how the system progressively narrows down options.

% Base case: If no restaurants left, set RecommendedRestaurant to empty list
ask_questions(Restaurants, RecommendedRestaurant) :-
    write('here2'),
    write(Restaurants),
    (Restaurants = [] ->
        write('here3'),
        RecommendedRestaurant = []
    ;
        write('here4'),
        % Select the next question to ask based on the remaining restaurants
        select_question(Restaurants, [], Question),
        write('asking questions'),
        write(Question),
        (Question = none ->
            % If no more questions to ask, set RecommendedRestaurant to the remaining restaurants
            RecommendedRestaurant = Restaurants
        ;
            % Ask the selected question and get the user's answer
            ask_question(Question, Answer),
            filter_restaurants(Restaurants, Question, Answer, FilteredRestaurants),
            write('done filtering'),
            write(FilteredRestaurants),
            (FilteredRestaurants = [Restaurant] ->
                % If only one restaurant left, set it as the recommended restaurant
                RecommendedRestaurant = [Restaurant]
            ;
                % Recursively ask questions with the filtered restaurants and the current question added to the asked questions list
                ask_questions(FilteredRestaurants, [Question], RecommendedRestaurant)
            )
        )
    ).

% Recursive case: Ask questions with the updated list of asked questions
ask_questions(Restaurants, AskedQuestions, RecommendedRestaurant) :-
    write('here2'),
    write(Restaurants),
    (Restaurants = [] ->
        write('here3'),
        RecommendedRestaurant = []
    ;
        write('here4'),
        % Select the next question based on the remaining restaurants and the asked questions
        select_question(Restaurants, AskedQuestions, Question),
        write('asking questions'),
        write(Question),
        (Question = none ->
            % If no more relevant questions to ask, set RecommendedRestaurant to the remaining restaurants
            RecommendedRestaurant = Restaurants
        ;
            % Ask the selected question and get the user's answer
            ask_question(Question, Answer),
            % Filter restaurants based on the answer
            filter_restaurants(Restaurants, Question, Answer, FilteredRestaurants),
            write('done filtering'),
            write(FilteredRestaurants),
            (FilteredRestaurants = [Restaurant] ->
                % If only one restaurant left, set it as the recommended restaurant
                RecommendedRestaurant = [Restaurant]
            ;
                % Add the current question to the asked questions list
                append(AskedQuestions, [Question], UpdatedAskedQuestions),
                % Recursively ask questions with the filtered restaurants and the updated asked questions list
                ask_questions(FilteredRestaurants, UpdatedAskedQuestions, RecommendedRestaurant)
            )
        )
    ).

% Predicate to determine the next relevant question to ask the user.
% Follows #cs152-search by selecting the most appropriate question, akin to a decision in search algorithms,
% and adheres to #cs152-ailogic by basing the selection on the current state of the system.

% Select the next question to ask based on the remaining restaurants and the asked questions
select_question(Restaurants, AskedQuestions, Question) :-
    write('here5'),
    % Find all cuisine values for the remaining restaurants
    findall(Cuisine, (member(Name, Restaurants), restaurant(Name, Cuisine, _, _, _, _, _, _, _)), Cuisines),
    write(Cuisines),
    write(Restaurants),
    % Remove duplicates to get unique cuisine values
    list_to_set(Cuisines, UniqueCuisines),
    % Count the number of unique cuisine values
    length(UniqueCuisines, CuisineCount),
    write('cuisine length'),
    write(CuisineCount),

    % Find all budget values for the remaining restaurants
    findall(Budget, (member(Name, Restaurants), restaurant(Name, _, Budget, _, _, _, _, _, _)), Budgets),
    write(Budgets),
    % Remove duplicates to get unique budget values
    list_to_set(Budgets, UniqueBudgets),
    % Count the number of unique budget values
    length(UniqueBudgets, BudgetCount),
    write('budget length'),
    write(BudgetCount),

    % Find all distance values for the remaining restaurants
    findall(Distance, (member(Name, Restaurants), restaurant(Name, _, _, Distance, _, _, _, _, _)),Distances),
    write(Distances),
    % Remove duplicates to get unique distance values
    list_to_set(Distances, UniqueDistances),
    % Count the number of unique distance values
    length(UniqueDistances, DistanceCount),
    write('distance length'),
    write(DistanceCount),

    % Find all dietary restriction values for the remaining restaurants
    findall(Restrictions, (member(Name, Restaurants), restaurant(Name, _, _, _, Restrictions, _, _, _, _)), AllRestrictions),
    flatten(AllRestrictions, FlatRestrictions),
    % Remove duplicates to get unique dietary restriction values
    list_to_set(FlatRestrictions, UniqueRestrictions),
    % Count the number of unique dietary restriction values
    length(UniqueRestrictions, RestrictionsCount),
    write('restrictions length'),
    write(RestrictionsCount),

    % Find all restaurant types values for the remaining restaurants
    findall(Type, (member(Name, Restaurants), restaurant(Name, _, _, _, _, Type, _, _, _)), Types),
    % Remove duplicates to get unique restaurant types values
    list_to_set(Types, UniqueTypes),
    % Count the number of unique restaurant types values
    length(UniqueTypes, TypeCount),
    write('rest type length'),
    write(TypeCount),

    % Find all food type values for the remaining restaurants
    findall(FoodType, (member(Name, Restaurants), restaurant(Name, _, _, _, _, _, FoodType, _, _)), FoodTypes),
    % Remove duplicates to get unique food type values
    list_to_set(FoodTypes, UniqueFoodTypes),
    % Count the number of unique food type values
    length(UniqueFoodTypes, FoodTypeCount),
    write('food type length'),
    write(FoodTypeCount),

    % Find all appetite values for the remaining restaurants
    findall(Appetite, (member(Name, Restaurants), restaurant(Name, _, _, _, _, _, _, Appetite, _)),Appetites),
    % Remove duplicates to get unique appetite values
    list_to_set(Appetites, UniqueAppetites),
    % Count the number of unique appetite values
    length(UniqueAppetites, AppetiteCount),
    write('appetites length'),
    write(AppetiteCount),

    % Find all meal type values for the remaining restaurants
    findall(MealType, (member(Name, Restaurants), restaurant(Name, _, _, _, _, _, _, _, MealType)), MealTypes),
    % Remove duplicates to get unique meal type values
    list_to_set(MealTypes, UniqueMealTypes),
    % Count the number of unique meal type values
    length(UniqueMealTypes, MealTypeCount),
    write('meal type length'),
    write(MealTypes),
    write(UniqueMealTypes),
    write(MealTypeCount),

    (CuisineCount > 1, \+ member(cuisine, AskedQuestions) ->
        % If there are multiple unique cuisine values and cuisine hasn't been asked, select cuisine as the next question
        Question = cuisine
    ;
        (BudgetCount > 1, \+ member(budget, AskedQuestions) ->
            % If there are multiple unique budget values and budget hasn't been asked, select budget as the next question
            Question = budget
        ;
            (DistanceCount > 1, \+ member(distance, AskedQuestions) ->
                Question = distance
            ;
                (RestrictionsCount > 1, \+ member(dietary_restrictions, AskedQuestions) ->
                    Question = dietary_restrictions
                ;
                    (TypeCount > 1, \+ member(restaurant_type, AskedQuestions) ->
                        Question = restaurant_type
                    ;
                        (FoodTypeCount > 1, \+ member(food_type, AskedQuestions) ->
                            Question = food_type
                        ;
                            (AppetiteCount > 1, \+ member(appetite, AskedQuestions) ->
                                Question = appetite
                            ;
                                (MealTypeCount > 1, \+ member(meal_type, AskedQuestions) ->
                                    Question = meal_type
                                ;
                                    Question = none
                                )
                            )
                        )
                    )
                )
            )
        )
    ).

% Ask the user for their preferred cuisine and map the user's choice to the corresponding cuisine value
ask_question(cuisine, Cuisine) :-
    write('What type of cuisine do you prefer?'), nl,
    write('1. African'), nl,
    write('2. North American'), nl,
    write('3. Asian'), nl,
    write('4. South American'), nl,
    write('5. Mediterranean'), nl,
    write('6. European'), nl,
    read(CuisineChoice),
    cuisine_option(CuisineChoice, Cuisine).

% Ask the user for their budget range and map the user's choice to the corresponding budget value
ask_question(budget, Budget) :-
    write('What is your budget range?'), nl,
    write('1. £1-10'), nl,
    write('2. £10-20'), nl,
    write('3. £20-30'), nl,
    write('4. £30-40'), nl,
    read(BudgetChoice),
    budget_option(BudgetChoice, Budget).

ask_question(distance, Distance) :-
    write('How far are you willing to travel (in miles)?'), nl,
    write('1. 0.5 miles'), nl,
    write('2. 1 mile'), nl,
    write('3. 5 miles'), nl,
    write('4. 10 miles'), nl,
    write('5. 15 miles'), nl,
    read(DistanceChoice),
    distance_option(DistanceChoice, Distance).

ask_question(dietary_restrictions, Restrictions) :-
    write('Do you have any dietary restrictions?'), nl,
    write('1. Vegetarian'), nl,
    write('2. Vegan'), nl,
    write('3. Halal'), nl,
    write('4. No restrictions'), nl,
    read(RestrictionsChoice),
    restrictions_option(RestrictionsChoice, Restrictions).

ask_question(restaurant_type, Type) :-
    write('Do you prefer chain or local restaurants?'), nl,
    write('1. Chain'), nl,
    write('2. Local'), nl,
    read(TypeChoice),
    restaurant_type_option(TypeChoice, Type).

ask_question(food_type, FoodType) :-
    write('Do you prefer healthy or fast food?'), nl,
    write('1. Healthy'), nl,
    write('2. Fast food'), nl,
    read(FoodTypeChoice),
    food_type_option(FoodTypeChoice, FoodType).

ask_question(appetite, Appetite) :-
    write('Are you looking for a full or light meal?'), nl,
    write('1. Full meal'), nl,
    write('2. Light meal'), nl,
    read(AppetiteChoice),
    appetite_option(AppetiteChoice, Appetite).

ask_question(meal_type, MealType) :-
    write('What type of meal are you looking for?'), nl,
    write('1. Breakfast'), nl,
    write('2. Lunch'), nl,
    write('3. Dinner'), nl,
    read(MealTypeChoice),
    meal_type_option(MealTypeChoice, MealType).

% Predicate to filter the list of restaurants based on user responses to a specific question.
% This highlights #cs152-ailogic by using conditional checks and list processing to modify the state of the system,
% aligning with the principles of logic programming.

% Filter the restaurants based on the selected question and answer
filter_restaurants(Restaurants, Question, Answer, FilteredRestaurants) :-
    % If the selected question is about cuisine
    (Question = cuisine ->
        % Find all restaurant names where the cuisine matches the answer
        findall(Name, (member(Name, Restaurants), restaurant(Name, Answer, _, _, _, _, _, _, _)), FilteredRestaurants)
    ;
        % If the selected question is about budget
        (Question = budget ->
            % Find all restaurant names where the budget matches the answer
            findall(Name, (member(Name, Restaurants), restaurant(Name, _, Answer, _, _, _, _, _, _)), FilteredRestaurants)
        ;
            % If the selected question is about distance
            (Question = distance ->
                % Find all restaurant names where the distance is less than or equal to the answer
                findall(Name, (member(Name, Restaurants), restaurant(Name, _, _, Distance, _, _, _, _, _), Distance =< Answer), FilteredRestaurants)
            ;
                % If the selected question is about dietary restrictions
                (Question = dietary_restrictions ->
                    % Find all restaurant names where the restrictions are a subset of the answer
                    findall(Name, (member(Name, Restaurants), restaurant(Name, _, _, _, Restrictions, _, _, _, _), subset(Answer, Restrictions)), FilteredRestaurants)
                ;
                    % If the selected question is about restaurant type
                    (Question = restaurant_type ->
                        % Find all restaurant names where the type matches the answer
                        findall(Name, (member(Name, Restaurants), restaurant(Name, _, _, _, _, Answer, _, _, _)), FilteredRestaurants)
                    ;
                        % If the selected question is about food type
                        (Question = food_type ->
                            % Find all restaurant names where the food type matches the answer
                            findall(Name, (member(Name, Restaurants), restaurant(Name, _, _, _, _, _, Answer, _, _)), FilteredRestaurants)
                        ;
                            % If the selected question is about appetite
                            (Question = appetite ->
                                % Find all restaurant names where the appetite matches the answer
                                findall(Name, (member(Name, Restaurants), restaurant(Name, _, _, _, _, _, _, Answer, _)), FilteredRestaurants)
                            ;
                                % If the selected question is about meal type
                                (Question = meal_type ->
                                    % Find all restaurant names where the meal type matches the answer
                                    findall(Name, (member(Name, Restaurants), restaurant(Name, _, _, _, _, _, _, _, Answer)), FilteredRestaurants)
                                ;
                                    % If none of the specific filters apply, return all restaurants unchanged
                                    FilteredRestaurants = Restaurants
                                )
                            )
                        )
                    )
                )
            )
        )
    ).

% Write the restaurant names in a list format
write_restaurant_list([]).
write_restaurant_list([Name | Rest]) :-
    write('- '), write(Name), nl,
    write_restaurant_list(Rest).

% Option predicates: Map the user's choice to the corresponding attribute value
cuisine_option(1, african).
cuisine_option(2, north_american).
cuisine_option(3, asian).
cuisine_option(4, south_american).
cuisine_option(5, mediterranean).
cuisine_option(6, european).

budget_option(1, '£1-10').
budget_option(2, '£10-20').
budget_option(3, '£20-30').
budget_option(4, '£30-40').

distance_option(1, 0.5).
distance_option(2, 1).
distance_option(3, 5).
distance_option(4, 10).
distance_option(5, 15).

restrictions_option(1, [vegetarian]).
restrictions_option(2, [vegan]).
restrictions_option(3, [halal]).
restrictions_option(4, []).

restaurant_type_option(1, chain).
restaurant_type_option(2, local).

food_type_option(1, healthy).
food_type_option(2, fast_food).

appetite_option(1, full).
appetite_option(2, light).

meal_type_option(1, breakfast).
meal_type_option(2, lunch).
meal_type_option(3, dinner).