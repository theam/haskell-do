'use strict'
const electron = require('electron')
const React = require('react')
const ReactDOM = require('react-dom')
const $ = require('jquery')
const jQuery = require('jquery')
const app = electron.app // this is our app
const BrowserWindow = electron.BrowserWindow // This is a Module that creates windows
var spawn = require('child_process').execSync;

let mainWindow // saves a global reference to mainWindow so it doesn't get garbage collected

app.on('ready', createMainWindow) // called when electron has initialized

function createMainWindow () {
  if (!stackIsOnPath()) {
    electron.dialog.showErrorBox({
      title:"HaskellDO initialization error",
      content:"Stack not found on path. Try adding it?"
    });
    app.exit(-1);
  }
  const {width, height} = electron.screen.getPrimaryDisplay().workAreaSize
  mainWindow = new BrowserWindow({width, height, show: false})
  mainWindow.loadURL(`file://${ __dirname }/index.html`)
  mainWindow.webContents.openDevTools()

  mainWindow.once('ready-to-show', () => {
    mainWindow.show()
  })

  mainWindow.on('closed', function () {
    mainWindow = null
  })

}
function stackIsOnPath () {
        try {
          var out = spawn('stack --help');
        } catch (error) {
          return false;
        }
        return true;
}

/* Mac Specific things */

// when you close all the windows on a non-mac OS it quits the app
app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') { app.quit() }
})

// if there is no mainWindow it creates one (like when you click the dock icon)
app.on('activate', () => {
  if (mainWindow === null) { createWindow() }
})
