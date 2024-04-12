from flask import Flask, jsonify, render_template, request
from pyswip import Prolog

app = Flask(__name__)
prolog = Prolog()
prolog.consult("rest_kb.pl")


@app.route("/")
def home():
    return render_template("index.html")


@app.route("/restaurants", methods=["GET"])
def get_restaurants():
    query = "findall(Name, restaurant(Name, _, _, _, _, _, _, _, _), Restaurants)"
    result = list(prolog.query(query))

    if result:
        restaurants = [str(r) for r in result[0]["Restaurants"]]
        return jsonify({"restaurants": restaurants})
    else:
        return jsonify({"restaurants": []})


@app.route("/question", methods=["POST"])
def get_question():
    restaurants = request.json["restaurants"]
    asked_questions = request.json["askedQuestions"]

    query = f"select_question({restaurants}, {asked_questions}, Question)"
    result = list(prolog.query(query))

    question = result[0]["Question"]

    if question != "none":
        return jsonify({"question": question})
    else:
        return jsonify({"question": None})


@app.route("/recommend", methods=["POST"])
def recommend():
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


if __name__ == "__main__":
    app.run(debug=True)
