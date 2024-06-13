# the necessary packages
library(highcharter)
library(shiny)
library(shinyanimate)
library(shinyBS)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)

games = read_csv('data/games.csv')
goals = read_csv('data/goals.csv')
shoot = read_csv('data/shoot.csv')
teams = read_csv('data/teams.csv') |> arrange(group, team)

hdrs = c(
  'Date',
  'Team 1',
  'Score',
  'Team 2',
  'Round'
)

wdths = c(2,3,2,3,2)

