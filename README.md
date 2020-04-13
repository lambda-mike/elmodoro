# Elmodoro

This project is a humble attempt to implement a simple
[Pomodoro technique](https://francescocirillo.com/pages/pomodoro-technique)
timer in Elm.

I have plans to create a course, where I will teach and explain how to create
this Elm web app step by step from first principles.

Let me know on [Twitter](https://twitter.com/LambdaMike) if you like the idea!

## Build

### Dev

With _watch_ mode:

`yarn dev`

`parcel --port 5314 index.html`

Only Elm:

`elm reactor`

And navigate to localhost:8000 and click _src/Main.elm_

### Prod

`yarn build`

`parcel build index.html`

Without source maps:

`parcel build --no-source-maps index.html`

