from flask import Flask, jsonify, render_template, request
from pyswip import Prolog

# Initialize Flask app
app = Flask(__name__)

# Initialize Prolog engine
prolog = Prolog()

# Load the Prolog knowledge base
prolog.consult("rest_kb.pl")


# Route to the home page
@app.route("/")
def home():
    """
    Renders the index.html template for the home page.
    """
    return render_template("index.html")


# Route to get a list of restaurants
@app.route("/restaurants", methods=["GET"])
def get_restaurants():
    """
    Retrieves a list of restaurants from the Prolog knowledge base.

    Returns:
        JSON response containing the list of restaurants.
    """
    query = "findall(Name, restaurant(Name, _, _, _, _, _, _, _, _), Restaurants)"
    result = list(prolog.query(query))

    if result:
        restaurants = [str(r) for r in result[0]["Restaurants"]]
        return jsonify({"restaurants": restaurants})
    else:
        return jsonify({"restaurants": []})


# Route to get a question based on previous answers
@app.route("/question", methods=["POST"])
def get_question():
    """
    Retrieves a question based on the current state of the conversation.

    Returns:
        JSON response containing the question.
    """
    restaurants = request.json["restaurants"]
    asked_questions = request.json["askedQuestions"]

    query = f"select_question({restaurants}, {asked_questions}, Question)"
    result = list(prolog.query(query))

    question = result[0]["Question"]

    if question != "none":
        return jsonify({"question": question})
    else:
        return jsonify({"question": None})


# Route to recommend restaurants based on user preferences
@app.route("/recommend", methods=["POST"])
def recommend():
    """
    Recommends restaurants based on user preferences.

    Returns:
        JSON response containing the recommended restaurant(s).
    """
    restaurants = request.json["restaurants"]
    question = request.json["question"]
    answer = request.json["answer"]

    query = f"filter_restaurants({restaurants}, {question}, '{answer}', FilteredRestaurants)"
    result = list(prolog.query(query))

    if result:
        filtered_restaurants = result[0]["FilteredRestaurants"]
        if len(filtered_restaurants) == 1:
            restaurant = filtered_restaurants[0]
            return jsonify({"recommendation": restaurant})
        else:
            return jsonify({"restaurants": filtered_restaurants})
    else:
        return jsonify({"recommendation": None})


# Run the Flask app
if __name__ == "__main__":
    app.run(debug=True)
