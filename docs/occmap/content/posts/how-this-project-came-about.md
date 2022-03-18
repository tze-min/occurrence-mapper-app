---
title: "How this project came about"
date: 2022-03-18T18:33:02+08:00
description: "and why I'm working on it again"
author: "Tze Min"
draft: false
categories: ["meta"]
tags: ["goals", "ideas"]
---

# Origin

A research lab I interned for in 2021 wanted to study the distribution of insect species in Singapore, given some occurrence datasets.[^1] 

Originally, I wrote a script to create interactive maps, because I wasn't happy with static ones. Displaying the occurrence data of more than one species, genus, or other higher levels of taxonomy complicated the maps; we couldn't tell the difference between the data points of multiple taxonomy groups. And manually generating a map per group? No. 

But `Leaflet`, an R package for developing interactive maps, let me do fun things like toggle between species, produce tooltips with additional information about each data point by hovering over it, and zoom into exactly what sort of habitat a specific organism was observed at.

However, I'd also been asked to visualise other fields the data had. Who were the top recorders over time? Which families and orders of species had the greatest number of observations? And what about the dataset itself: how many missing values? How much more data -- i.e. recording observations via field trips -- do we need to collect before we can say, "Okay, this dataset fairly represents the species available in this area of study?"

So I proposed this dashboard app written in `Shiny` that provides both interactive maps and summary statistics, and because prior to that I've never coded out an app (by my lonesome) in my life, proceeded to spend the rest of my internship working on it.

# Why work on it again?

I've interning elsewhere now, six months later, worlds away from academia and biodiversity labs. But I look back at the time I spent exploring packages and studying how reactivity worked in `Shiny` and worrying hair ends about why the pickers on my maps weren't working the exact way I wanted them to -- I thought:

1. Wow I had fun!
2. I can still make a lot of improvements -- and test out ideas

I don't necessarily *have* to look at wildlife within Singapore. Databases like GBIF exist out there that I've barely touched, and I want to see what insights they can give me. What have people been doing with species occurrence data? Surely more than putting them into maps. And plotting individual points like these in the app works well when the data displayed per species is sparse, but what about when there's plenty? What about when you want to bring in environmental factors -- how could that look and fit into an app?

Not to mention the bugs. And making the app's features more convenient, if not for others, then at least for myself. One of the biggest things I took away from my internship at the lab was to take pride in my own work, and yeah, sometimes I wonder if building a dashboard app is something others in my faculty (computing) find simple, but I still remember the process of doing so, of learning through encountering bug by frustrating bug, of watching the product of your work fill out, take shape. 

I'd like to keep a long-term project of experiencing this again.

# My long-term goals

## 1. Test out my ideas

I've been brainstorming them [here](https://github.com/tze-min/occurrence-mapper-app/projects/1), and will probably continue doing so. I'd consider this a success if I've given at least each item in the kanban board a go.

## 2. Practice blogging and documenting

Because how else am I going to remember what I've done? Besides, I like writing. I've just never done it publicly for a coding project before. (I also like biodiversity and the environment. Intersecting writing x programming x environmental studies for a career or a hobby would literally be the biggest win.)

I'd consider this goal a success if I can turn to my previous posts to recall what I've learned, say, another six months down the road. No need to be too consistent with posting since I need to prioritise my other commitments, but I want my posts over time to be a reflection of how I've grown.

## So, two big things

...that I'm aiming for. But in setting up this project site and writing this post, I've already tested out a couple of ideas, learned more about GitHub Pages and how hosting Hugo sites work, and started my blogging practice. That's more than where I'd been at before.

[^1]: We can think of occurrence data as rows of observations of organisms, each with the organism's scientific name, coordinates and datetime the recorder observed it, at the very least.