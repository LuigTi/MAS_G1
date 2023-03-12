# VINCENZO, planning and searching cooking assistant



![image](https://user-images.githubusercontent.com/91637040/224562243-6017cdc2-53d3-48bb-9a58-b34ab52edc14.png)

The idea followed by the team was to implement a rule-based cooking assistant and implement query and visual features to add planning capabilities; that is, the assistant had the extended capability of planning the weekly diet of the user and its grocery list. The calendar and the list can be modified dynamically through conversation. Once the planned calendar and the grocery list are satisfied, the user can print the information. Furthermore, a user study was carried out to make an informed decision about the design of the prototype and its conversation patterns. 



### Implementing Rule-Based Agent for VU course PMAS (Description of the task of the course=

In this project, you will be developing a conversational recipe recommendation agent that uses speech to interact and is able to conduct a conversation for selecting a recipe to cook.  Your agent should be able to assist a user in selecting a recipe using a variety of filters. The agent does not need to be able to assist a user with the instruction steps of the recipe itself, which is out of the scope of the Project MAS course. We chose to focus on the recipe selection activity since it already poses several challenges for building an effective and robust conversational agent. First, there are many different ways in which this conversation may be conducted and many different ways in which a user can phrase what they want from the agent. A user can specify different aspects of a recipe that the recipe it will finally select should satisfy (e.g., type of ingredients, cooking duration, type of course, etc.). Second, the recipe recommendation domain already is a broad knowledge space that the agent needs to be able to handle to understand what the user is looking for. The agent will have to reason over its database of recipes to filter for recipes that fit the user’s preferences.

You are provided with a Prolog knowledge base of close to 1,000 recipes and their components, which still requires an effort from your group to make it usable by the recipe recommendation agent. In the instructions, we walk you through the procedure by pinpointing aspects of the agent that need to be altered or filled in. If you fill in these blanks, the agent should work, but it will still be pretty basic… 

This repository includes an agent code, an Entity directory, and an Intent directory.

### CREDIT AND UTILS

GUIDE HOW TO INSTALL: https://socialrobotics.atlassian.net/wiki/spaces/MAS23/pages/2155839491/Getting+Started+What+You+Need+to+Install

CREDIT: https://github.com/PMAS-2023 originally forked from the course repository but the GitHub classroom had a members' limit so we had to manually import it
