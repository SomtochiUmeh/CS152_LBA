% Restaurant knowledge base

% Define the attributes of every restaurant in the knowledge base.
% Each restaurant is represented by a unique predicate with its name and various attributes such as cuisine type,
% budget range, distance from the residence hall), dietary restrictions, the type of establishment (chain or local),
% the type of food (healthy or fast food), the size of the meal (full or light), the meal it caters to (breakfast, lunch, dinner, or snack).
% This section applies #rightproblem by clearly defining the problem space 
% and #sourcequality by ensuring each restaurant's attributes are accurately represented.

restaurant('Aso Rock Restaurant', african, '£10-20', 15, [], local, fast_food, full, dinner).
restaurant('Bankes Kitchen', american, '£10-20', 1.5, [], local, fast_food, full, lunch).
restaurant('T4 Boba', asian, '£1-10', 0.4, [vegan], local, healthy, light, snack).
restaurant('Honest Burgers Old Street', american, '£10-20', 0.1, [vegetarian], chain, fast_food, full, lunch).
restaurant('Nandos Old Street', african, '£10-20', 0.2, [], chain, fast_food, full, lunch).
restaurant('The Old Street Chinese Restaurant', chinese, '£20-30', 0.2, [], local, healthy, full, dinner).
restaurant('Danyames Kitchen The Kenny House', african, '£1-10', 11, [], local, healthy, full, lunch).
restaurant('Sea Garden and Grill', seafood, '£30-40', 7.6, [], local, healthy, full, dinner).
restaurant('Cooked by B', american, '£10-20', 7.4, [], local, healthy, full, lunch).
restaurant('Açaí Berry Wardour St.', healthy, '£10-20', 2.2, [vegan, gluten_free], local, healthy, light, snack).
restaurant('Agege Bread London Bakers', bakery, '£10-20', 4.5, [vegetarian], local, healthy, light, snack).
restaurant('The Best Kebab', mediterranean, '£1-10', 0.4, [], local, fast_food, full, lunch).
restaurant('Hoxton Grill', american, '£20-30', 0.3, [], local, healthy, full, dinner).
restaurant('The Breakfast Club Spitalfields', american, '£10-20', 0.8, [vegetarian], chain, fast_food, full, breakfast).

% Askables

% Define the askable predicates which represent the attributes that the expert system can query about.
% This approach applies #cs152-ailogic by using predicate logic to describe relationships and
% applies #rightproblem by characterizing the attributes that will inform the system's decision-making.

cuisine(Cuisine) :- member(Cuisine, [african, american, asian, chinese, seafood, healthy, bakery, mediterranean, convenience]).
budget(Budget) :- member(Budget, ['£1-10', '£10-20', '£20-30', '£30-40']).
distance(Distance) :- member(Distance, [0.5, 1, 5, 10, 15]).
dietary_restrictions(Restrictions) :- subset(Restrictions, [vegetarian, vegan, gluten_free]).
restaurant_type(Type) :- member(Type, [chain, local]).
food_type(FoodType) :- member(FoodType, [healthy, fast_food]).
appetite(Appetite) :- member(Appetite, [full, light]).
meal_type(MealType) :- member(MealType, [breakfast, lunch, dinner, snack]).

% User interaction

% The entry point for user interaction with the expert system.
% Implements #cs152-aicoding by invoking the AI algorithm within a Prolog environment.
% Utilizes a clear approach to querying the user (#professionalism) and a structured method (#modeling)
% to progress from a general pool of restaurants to a personalized recommendation.

recommend_restaurant :-
    % Retrieve all restaurant names to form the initial pool of choices.
    % Demonstrates effective use of Prolog's findall predicate for initial data collection (#evidencebased).
    findall(Name, restaurant(Name, _, _, _, _, _, _, _, _), Restaurants),
    % Initiate the process of refining the restaurant list based on user responses.
    ask_questions(Restaurants, RecommendedRestaurant),
    (RecommendedRestaurant = [] ->
        fail
    ;
        true
    ).

% Recursive predicate that filters the list of potential restaurants based on user input.
% Illustrates #aicoding by implementing recursion and conditional logic to refine user choices.
% Also demonstrates #modeling by showing how the system progressively narrows down options.
ask_questions(Restaurants, RecommendedRestaurant) :-
    (Restaurants = [] ->
        RecommendedRestaurant = []
    ;
        select_question(Restaurants, [], Question),
        (Question = none ->
            RecommendedRestaurant = Restaurants
        ;
            filter_restaurants(Restaurants, Question, FilteredRestaurants),
            (FilteredRestaurants = [Restaurant] ->
                RecommendedRestaurant = [Restaurant]
            ;
                ask_questions(FilteredRestaurants, [Question], RecommendedRestaurant)
            )
        )
    ).

ask_questions(Restaurants, AskedQuestions, RecommendedRestaurant) :-
    (Restaurants = [] ->
        RecommendedRestaurant = []
    ;
        select_question(Restaurants, AskedQuestions, Question),
        (Question = none ->
            RecommendedRestaurant = Restaurants
        ;
            filter_restaurants(Restaurants, Question, FilteredRestaurants),
            (FilteredRestaurants = [Restaurant] ->
                RecommendedRestaurant = [Restaurant]
            ;
                append(AskedQuestions, [Question], UpdatedAskedQuestions),
                ask_questions(FilteredRestaurants, UpdatedAskedQuestions, RecommendedRestaurant)
            )
        )
    ).

% Predicate to determine the next relevant question to ask the user.
% Follows #cs152-search by selecting the most appropriate question, akin to a decision in search algorithms,
% and adheres to #cs152-ailogic by basing the selection on the current state of the system.
select_question(Restaurants, AskedQuestions, Question) :-
    findall(Cuisine, (member(Name, Restaurants), restaurant(Name, Cuisine, _, _, _, _, _, _, _)), Cuisines),
    list_to_set(Cuisines, UniqueCuisines),
    length(UniqueCuisines, CuisineCount),

    findall(Budget, (member(Name, Restaurants), restaurant(Name, _, Budget, _, _, _, _, _, _)), Budgets),
    list_to_set(Budgets, UniqueBudgets),
    length(UniqueBudgets, BudgetCount),

    findall(Distance, (member(Name, Restaurants), restaurant(Name, _, _, Distance, _, _, _, _, _)),Distances),
    list_to_set(Distances, UniqueDistances),
    length(UniqueDistances, DistanceCount),

    findall(Restrictions, (member(Name, Restaurants), restaurant(Name, _, _, _, Restrictions, _, _, _, _)), AllRestrictions),
    flatten(AllRestrictions, FlatRestrictions),
    list_to_set(FlatRestrictions, UniqueRestrictions),
    length(UniqueRestrictions, RestrictionsCount),

    findall(Type, (member(Name, Restaurants), restaurant(Name, _, _, _, _, Type, _, _, _)), Types),
    list_to_set(Types, UniqueTypes),
    length(UniqueTypes, TypeCount),

    findall(FoodType, (member(Name, Restaurants), restaurant(Name, _, _, _, _, _, FoodType, _, _)), FoodTypes),
    list_to_set(FoodTypes, UniqueFoodTypes),
    length(UniqueFoodTypes, FoodTypeCount),

    findall(Appetite, (member(Name, Restaurants), restaurant(Name, _, _, _, _, _, _, Appetite, _)),Appetites),
    list_to_set(Appetites, UniqueAppetites),
    length(UniqueAppetites, AppetiteCount),

    findall(MealType, (member(Name, Restaurants), restaurant(Name, _, _, _, _, _, _, _, MealType)), MealTypes),
    list_to_set(MealTypes, UniqueMealTypes),
    length(UniqueMealTypes, MealTypeCount),

    (CuisineCount > 1, \+ member(cuisine, AskedQuestions) ->
        Question = cuisine
    ;
        (BudgetCount > 1, \+ member(budget, AskedQuestions) ->
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

% Predicate to filter the list of restaurants based on user responses to a specific question.
% This highlights #cs152-ailogic by using conditional checks and list processing to modify the state of the system,
% aligning with the principles of logic programming.
filter_restaurants(Restaurants, Question, Answer, FilteredRestaurants) :-
    (Question = cuisine ->
        findall(Name, (member(Name, Restaurants), restaurant(Name, Answer, _, _, _, _, _, _, _)), FilteredRestaurants)
    ;
        (Question = budget ->
            findall(Name, (member(Name, Restaurants), restaurant(Name, _, Answer, _, _, _, _, _, _)), FilteredRestaurants)
        ;
            (Question = distance ->
                atom_number(Answer, Distance),
                findall(Name, (member(Name, Restaurants), restaurant(Name, _, _, RestDistance, _, _, _, _, _), RestDistance =< Distance), FilteredRestaurants)
            ;
                (Question = dietary_restrictions ->
                    (Answer = none ->
                        Restrictions = []
                    ;
                        Restrictions = [Answer]
                    ),
                    findall(Name, (member(Name, Restaurants), restaurant(Name, _, _, _, RestRestrictions, _, _, _, _), subset(Restrictions, RestRestrictions)), FilteredRestaurants)
                ;
                    (Question = restaurant_type ->
                        findall(Name, (member(Name, Restaurants), restaurant(Name, _, _, _, _, Answer, _, _, _)), FilteredRestaurants)
                    ;
                        (Question = food_type ->
                            findall(Name, (member(Name, Restaurants), restaurant(Name, _, _, _, _, _, Answer, _, _)), FilteredRestaurants)
                        ;
                            (Question = appetite ->
                                findall(Name, (member(Name, Restaurants), restaurant(Name, _, _, _, _, _, _, Answer, _)), FilteredRestaurants)
                            ;
                                (Question = meal_type ->
                                    findall(Name, (member(Name, Restaurants), restaurant(Name, _, _, _, _, _, _, _, Answer)), FilteredRestaurants)
                                ;
                                    FilteredRestaurants = Restaurants
                                )
                            )
                        )
                    )
                )
            )
        )
    ).
