# Restaurant Recommendation Expert System

This repository contains an expert system for recommending restaurants based on user preferences. The system is implemented using Prolog for the knowledge base and reasoning, and Flask for the web-based user interface.

## Table of Contents

- [Introduction](#introduction)
- [Features](#features)
- [Installation](#installation)
- [Usage](#usage)
  - [GUI](#gui)
  - [CLI](#cli)
  - [Video Demo](#video-demo)
- [File Structure](#file-structure)
- [Contributing](#contributing)
- [License](#license)

## Introduction

The Restaurant Recommendation Expert System helps users find suitable restaurants based on their preferences such as cuisine type, budget range, distance, dietary restrictions, restaurant type, food type, appetite, and meal type. The system uses a knowledge base of restaurants and their attributes, and applies logical reasoning to narrow down the options and provide personalized recommendations.

## Features

- Prolog-based knowledge base and reasoning
- Flask-based web interface for user interaction
- Interactive questioning to gather user preferences
- Personalized restaurant recommendations based on user input
- Support for both GUI and CLI modes

## Installation

1. Clone the repository:

```
git clone https://github.com/SomtochiUmeh/CS152_LBA.git
```

2. Install the required dependencies:

```
cd restaurant_recommendation
pip install -r requirements.txt
```

## Usage

### GUI

1. Start the Flask server:

```
cd restaurant_recommendation
python -m venv venv
source venv/bin/activate
python app.py
```

2. Open a web browser and navigate to `http://localhost:5000`.

3. Follow the prompts and answer the questions based on your preferences.

4. The system will provide personalized restaurant recommendations based on your input.

### CLI

1. Start the Prolog interpreter:

```
brew install swi-prolog
swipl
```

2. Load the knowledge base:

```
cd LBA_group
?- [kb].
```

3. Run the `recommend_restaurant` predicate:

```
?- recommend_restaurant.
```

4. Follow the prompts and enter your preferences.

5. The system will provide personalized restaurant recommendations based on your input.

### Video Demo

https://github.com/SomtochiUmeh/CS152_LBA/assets/74836691/366aec6f-5090-4f46-bf40-d7739f0dd704


## File Structure

- `restaurant_recommendation/rest_kb.pl`: Prolog knowledge base containing restaurant information and reasoning logic for GUI.
- `restaurant_recommendation/app.py`: Flask application for the web-based user interface.
- `restaurant_recommendation/templates/index.html`: HTML template for the user interface.
- `restaurant_recommendation/static/index.css`: CSS stylesheet for the user interface.
- `KB.pl`: Prolog knowledge base containing restaurant information and reasoning logic for CLI.
- `README.md`: Project documentation and instructions.
