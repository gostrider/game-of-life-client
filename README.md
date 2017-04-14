# A Game of Life Client written in Elm


## Requirement:

elm version  
elm 0.18

For macOS,

Download installer or install from `npm install -g elm`

For ubuntu,

To install node package manger  
`sudo apt-get install npm`

To install elm  
`npm install -g elm`


## Build:

To download project required elm packages  
`elm-package install`

To build project  
`elm-make src/native/Main.elm --output index.html`


## Description:

This project is aim to implement Conway’s Game of Life client using elm.

Elm is a functional language that compiles to JavaScript.  
It competes with projects like React as a tool for creating websites and web apps.

This project follows [TEA](https://guide.elm-lang.org/architecture/) architecture and Module separated by MVC architecture.

## Features completed:
- User connect to server using web socket
- User able to select different cells on the gird
- User able to sends the selected cells to game server
- User able to reset the game
- User able to receive the next cells based on selected cells
- Multiple users are able to view the synchronise clock of the game

## TODO features:
- Render game server results and show on the grid
- User able to see different colors on the grid
- User has their own represent color
- Multiple users can view synchronise cells from server


## Others
Another chat room application also built with elm, [Here](https://github.com/gostrider/chat_elm)
