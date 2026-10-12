# Preference List Review

An R Shiny app for refining a scouting preference list through head-to-head player comparisons.

**Open the app:** https://01a131fd-5e63-b8bf-47a0-8f87cbcd00cb.share.connect.posit.cloud

**Portfolio:** https://charliewiebe.github.io

The original version was used by scouts ahead of the Rule 4 Draft to refine their preference lists. This recreated version loads follow reports I've written on high school players, so you can read the reports and, if you know the players, rank them yourself and see where our evaluations differ.

The app runs on a free hosting tier that sleeps when idle, so the first load can take up to a minute.

## How it works

The review starts from the existing list and only asks about players who sit next to each other, so every answer is a decision a scout would actually make.

1. The app shows a random pair of neighboring players, side by side with their reports, in random left/right order.
2. If you prefer the player already ranked higher, nothing moves and the app picks another neighboring pair.
3. If you prefer the lower-ranked player, the two swap. The winner then keeps facing the player above until they lose, and the player they passed keeps facing the player below until they win. That settles both players' new spots before the review moves on.
4. The review ends once every neighboring pair in the current order has been decided, or earlier if you choose Finish Early.

The results page shows the updated list, the biggest movers and a CSV download. You can pick with the left and right arrow keys, and hide the reports with the toggle above the cards.

## Repository layout

```
app.R                 the Shiny app
follow_reports.csv    player name, school and follow report
manifest.json         package list for Posit Connect Cloud
```

## Run it locally

Install the packages once, then run the app from this folder:

```r
install.packages(c("shiny", "bslib", "DT", "dplyr", "stringr", "readr"))
shiny::runApp()
```
