
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Ecology 8540 Computational Workshops

May 2022  
Odum School of Ecology

The 2022 Ecology 8540 Computational Workshops are intensive modules
providing students with the skills needed to organize, analyze and
visualize data. This repository contains course materials for Module 2:
“Data visualization for ecology & epidemiology”

To register: <ideas@uga.edu>

### Module 2: Data visualization for ecology & epidemiology

CRN 63694

Monday, May 23, 9am-5pm  
Tuesday, May 24, 9am-5pm  
Wednesday, May 25, 9am-12pm

INSTRUCTORS  
John Drake (<jdrake@uga.edu>)  
Éric Marty (<emarty@uga.edu>)

This module presents basic principles and best practices for data
visualization in R, with examples drawn from ecology and epidemiology.
Topics include visualizing high-dimensional data, mapping, color,
interactive visualizations, and workflows for publication-ready figures.

## Learning objectives

## Schedule

**DAY 1: Monday, May 23**

09:00 Lecture:
[Introduction](https://github.com/CEIDatUGA/ECOL8540-datavis/blob/master/lectures/1_introduction.html)  
09:40 Excercise: [Exercise 1: Figure redesign](https://github.com/CEIDatUGA/ECOL8540-datavis/blob/master/exercises/1_redesign.pdf)  
10:20 Break

10:30 Lecture: [The Image Model]()  
11:10 Excercise: [Exercise 2: Time series](https://github.com/CEIDatUGA/ECOL8540-datavis/blob/master/exercises/2_timeseries.pdf)  
11:50 Wrap-up

12:00 Lunch

01:30 Lecture: [Chart
types](https://github.com/CEIDatUGA/ECOL8540-datavis/blob/master/lectures/3_chart_types.html)  
02:10 Excercise: [Exercise 3: High-dimensional data]()  
02:50 Break

03:00 Group projects

**DAY 2: Tuesday, May 24**

09:00 Lecture: [Aesthetics & Color]()  
09:40 Excercise: [Exercise 4]()  
10:20 Break

10:30 Lecture: [Graphic Perception]()  
11:10 Excercise: [Exercise 5]()  
11:50 Wrap-up

12:00 Lunch

01:30 Lecture: [Mapping]()  
02:10 Excercise: [Exercise
6](https://github.com/CEIDatUGA/ECOL8540-datavis/blob/master/lectures/6_maps.html)  
02:50 Break

03:00 Group projects

**DAY 3: Wednesday, May 25**

09:00 Group projects  
10:30 Break  
10:40 Presentation of group projects

## Lectures

1.  Introduction (Drake)  
2.  Data & Image Models (Marty)
3.  Chart types (Drake)
4.  Aesthetics & Color (Marty)
5.  Perception (Marty)
6.  Mapping (Drake)

## Exercises

1.  Figure redesign (no code) (Eric)  
2.  Annotated time series / multi time series (Eric)  
3.  High-dimensional data (without color scale if possible) (Eric)  
4.  Color excercise (Eric, John will give reference paper)  
    2d heatmap. Issues: upper triangle, clustering, ordering, x = beta,
    y = virulence (model with disease induced mortality), R0 = color  
5.  Workflows (Eric)  
6.  Map (john)

## Group projects (John)

5 groups of 4, 6 topics:

-   COVID-19: Compare county and state-level patterns over time for the
    state of GA  
-   COVID-19: Compare cases and mortality over time  
-   West nile virus: County level virus data over time
-   Lime disease: advance in last 2 decades
-   Stochastic SIR model:
    -   summarize epidemic trajectories, spaghetti plots, confidence
        intervals, distributions, etc.
-   ?

## Preparing Rstudio

    install.packages("magrittr")
    install.packages("knitr")
    install.packages("kableExtra")
