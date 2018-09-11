import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

var app = Elm.Main.init({
  node: document.getElementById('root')
});

app.ports.sync.subscribe(function () {
  const socket = new WebSocket('ws://localhost:1707/sync')
  socket.addEventListener('message', function (event) {
    const data = JSON.parse(event.data)
    if (data.status === 'done') {
      socket.close()
      app.ports.syncDone.send(true)
    } else if (data.progress) {
      app.ports.syncProgress.send(data.progress)
    }
  })

});

registerServiceWorker();
