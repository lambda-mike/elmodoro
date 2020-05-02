import './styles.css'
import { Elm } from './Main.elm'

(function () {
  const storageKey = 'elmodoro'
  const timerModelFallback = ''

  function loadTimerModel() {
    try {
      return localStorage.getItem(storageKey) || timerModelFallback
    }
    catch (err) {
      console.error('loadTimer error: ', err)
      return timerModelFallback
    }
  }

  const app = Elm.Main.init({
    node: document.querySelector('main'),
    flags: {
      width: window.innerWidth || 360,
      height: window.innerHeight || 640,
      timer: loadTimerModel()
    }
  })

  app.ports.saveTimerModel.subscribe(function(timer) {
    try {
      localStorage.setItem(storageKey, timer || timerModelFallback)
    }
    catch (err) {
      console.error('saveTimerModel error: ', err)
    }
  })

  app.ports.playSound.subscribe(function() {
    const bell = document.querySelector('#audio-bell')
    if (!bell) {
      console.error('Audio element not found')
      return
    }
    if (bell && typeof bell.play === 'function') {
      try {
        bell.play()
      }
      catch (err) {
        console.error('Erron when playing sound', err)
      }
    }
  })

})()
