% Restaurant knowledge base
restaurant('Aso Rock Restaurant', african, '£10-20', 15, [], local, fast_food, full, dinner).
restaurant('Sana Restaurant', african, '£10-20', 15, [], local, fast_food, full, dinner).
restaurant('Banker Kitchen', american, '£10-20', 1.5, [], local, fast_food, full, lunch).
restaurant('T4 Boba', asian, '£1-10', 0.4, [vegan], local, healthy, light, snack).
restaurant('Honest Burgers Old Street', american, '£10-20', 0.1, [vegetarian], chain, fast_food, full, lunch).
restaurant('Nando\'s Old Street', african, '£10-20', 0.2, [], chain, fast_food, full, lunch).
restaurant('The Old Street Chinese Rest', chinese, '£20-30', 0.2, [], local, healthy, full, dinner).
restaurant('Danyame\'S Kitchen - The Kenny House', african, '£1-10', 11, [], local, healthy, full, lunch).
restaurant('Sea Garden and Grill', seafood, '£30-40', 7.6, [], local, healthy, full, dinner).
restaurant('Cooked by B', american, '£10-20', 7.4, [], local, healthy, full, lunch).
restaurant('Açaí Berry Wardons Sir.', healthy, '£10-20', 2.2, [vegan, gluten_free], local, healthy, light, snack).
restaurant('Agee Bread London Bakes', bakery, '£10-20', 4.5, [vegetarian], local, healthy, light, snack).
restaurant('The Best Kebab', mediterranean, '£1-10', 0.4, [], local, fast_food, full, lunch).
restaurant('Sainsbury\'s Local Meal deal', convenience, '£1-10', 0.2, [], chain, fast_food, light, snack).
restaurant('Hoxton Grill', american, '£20-30', 0.3, [], local, healthy, full, dinner).
restaurant('The Breakfast Club Spitalfields', american, '£10-20', 0.8, [vegetarian], chain, fast_food, full, breakfast).

% Askables
cuisine(Cuisine) :- member(Cuisine, [african, american, asian, chinese, seafood, healthy, bakery, mediterranean, convenience]).
budget(Budget) :- member(Budget, ['£1-10', '£10-20', '£20-30', '£30-40']).
distance(Distance) :- member(Distance, [0.5, 1, 5, 10, 15]).
dietary_restrictions(Restrictions) :- subset(Restrictions, [vegetarian, vegan, gluten_free]).
restaurant_type(Type) :- member(Type, [chain, local]).
food_type(FoodType) :- member(FoodType, [healthy, fast_food]).
appetite(Appetite) :- member(Appetite, [full, light]).
meal_type(MealType) :- member(MealType, [breakfast, lunch, dinner, snack]).

% User interaction
trace.
recommend_restaurant :-
    write('here'),
    findall(Name, restaurant(Name, _, _, _, _, _, _, _, _), Restaurants),
    write(Restaurants),
    ask_questions(Restaurants, RecommendedRestaurant),
    (RecommendedRestaurant = [] ->
        write('No restaurant found that matches your preferences.')
    ;
        write('Based on your preferences, we recommend:'), nl,
        write(RecommendedRestaurant),
        write_restaurant_list(RecommendedRestaurant)
    ).

ask_questions(Restaurants, RecommendedRestaurant) :-
    write('here2'),
    write(Restaurants),
    (Restaurants = [] ->
        write('here3'),
        RecommendedRestaurant = []
    ;
        write('here4'),
        select_question(Restaurants, [], Question),
        write('asking questions'),
        write(Question),
        (Question = none ->
            RecommendedRestaurant = Restaurants
        ;
            ask_question(Question, Answer),
            filter_restaurants(Restaurants, Question, Answer, FilteredRestaurants),
            write('done filtering'),
            write(FilteredRestaurants),
            (FilteredRestaurants = [Restaurant] ->
                RecommendedRestaurant = [Restaurant]
            ;
                ask_questions(FilteredRestaurants, [Question], RecommendedRestaurant)
            )
        )
    ).

ask_questions(Restaurants, AskedQuestions, RecommendedRestaurant) :-
    write('here2'),
    write(Restaurants),
    (Restaurants = [] ->
        write('here3'),
        RecommendedRestaurant = []
    ;
        write('here4'),
        select_question(Restaurants, AskedQuestions, Question),
        write('asking questions'),
        write(Question),
        (Question = none ->
            RecommendedRestaurant = Restaurants
        ;
            ask_question(Question, Answer),
            filter_restaurants(Restaurants, Question, Answer, FilteredRestaurants),
            write('done filtering'),
            write(FilteredRestaurants),
            (FilteredRestaurants = [Restaurant] ->
                RecommendedRestaurant = [Restaurant]
            ;
                append(AskedQuestions, [Question], UpdatedAskedQuestions),
                ask_questions(FilteredRestaurants, UpdatedAskedQuestions, RecommendedRestaurant)
            )
        )
    ).

select_question(Restaurants, AskedQuestions, Question) :-
    write('here5'),
    findall(Cuisine, (member(Name, Restaurants), restaurant(Name, Cuisine, _, _, _, _, _, _, _)), Cuisines),
    write(Cuisines),
    write(Restaurants),
    list_to_set(Cuisines, UniqueCuisines),
    length(UniqueCuisines, CuisineCount),

    findall(Budget, (member(Name, Restaurants), restaurant(Name, _, Budget, _, _, _, _, _, _)), Budgets),
    write(Budgets),
    list_to_set(Budgets, UniqueBudgets),
    length(UniqueBudgets, BudgetCount),

    findall(Distance, (member(Name, Restaurants), restaurant(Name, _, _, Distance, _, _, _, _, _)),Distances),
    write(Distances),
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

    findall(MealType, (member(Name, Restaurants), restaurant(_, _, _, _, _, _, _, _, MealType)), MealTypes),
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

ask_question(cuisine, Cuisine) :-
    write('What type of cuisine do you prefer?'), nl,
    write('1. African'), nl,
    write('2. American'), nl,
    write('3. Asian'), nl,
    write('4. Chinese'), nl,
    write('5. Seafood'), nl,
    write('6. Healthy'), nl,
    write('7. Bakery'), nl,
    write('8. Mediterranean'), nl,
    write('9. Convenience'), nl,
    read(CuisineChoice),
    cuisine_option(CuisineChoice, Cuisine).

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
    write('3. Gluten-free'), nl,
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
    write('Are you looking for a full meal or light snacks?'), nl,
    write('1. Full meal'), nl,
    write('2. Light snacks'), nl,
    read(AppetiteChoice),
    appetite_option(AppetiteChoice, Appetite).

ask_question(meal_type, MealType) :-
    write('What type of meal are you looking for?'), nl,
    write('1. Breakfast'), nl,
    write('2. Lunch'), nl,
    write('3. Dinner'), nl,
    write('4. Snack'), nl,
    read(MealTypeChoice),
    meal_type_option(MealTypeChoice, MealType).

filter_restaurants(Restaurants, Question, Answer, FilteredRestaurants) :-
    (Question = cuisine ->
        findall(Name, (member(Name, Restaurants), restaurant(Name, Answer, _, _, _, _, _, _, _)), FilteredRestaurants)
    ;
        (Question = budget ->
            findall(Name, (member(Name, Restaurants), restaurant(Name, _, Answer, _, _, _, _, _, _)), FilteredRestaurants)
        ;
            (Question = distance ->
                findall(Name, (member(Name, Restaurants), restaurant(Name, _, _, Distance, _, _, _, _, _), Distance =< Answer), FilteredRestaurants)
            ;
                (Question = dietary_restrictions ->
                    findall(Name, (member(Name, Restaurants), restaurant(Name, _, _, _, Restrictions, _, _, _, _), subset(Answer, Restrictions)), FilteredRestaurants)
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

write_restaurant_list([]).
write_restaurant_list([Name | Rest]) :-
    write('- '), write(Name), nl,
    write_restaurant_list(Rest).

% Option predicates
cuisine_option(1, african).
cuisine_option(2, american).
cuisine_option(3, asian).
cuisine_option(4, chinese).
cuisine_option(5, seafood).
cuisine_option(6, healthy).
cuisine_option(7, bakery).
cuisine_option(8, mediterranean).
cuisine_option(9, convenience).

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
restrictions_option(3, [gluten_free]).
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
meal_type_option(4, snack).