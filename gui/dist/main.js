'use strict'
const os = require('os')
const child_process = require('child_process')
const electron = require('electron')
const React = require('react')
const ReactDOM = require('react-dom')
const $ = require('jquery')
const jQuery = require('jquery')
const app = electron.app // this is our app
const BrowserWindow = electron.BrowserWindow // This is a Module that creates windows
const appRoot = require('app-root-path');
var syncspawn = require('child_process').execSync;
var spawn = require('child_process').exec;
var coreProcess;

let mainWindow // saves a global reference to mainWindow so it doesn't get garbage collected

function devModeActivated() {
  process.argv.forEach(arg => {
    if (arg.includes("dev")){
      return true
    }
  })
  return false
}

app.on('ready', initApplication) // called when electron has initialized

function showStackNotOnPathError(){
    electron.dialog.showErrorBox({
      title:"HaskellDO initialization error",
      content:"Stack not found on path. Try adding it?"
    });
    app.exit(-1);
}

function openFile() {
  return electron.dialog.showOpenDialog({
    title: "HaskellDO - Open stack project",
    properties: ['openDirectory'],
  })
}

function openFileOrDie() {
  var filePath = ""
  var paths = openFile()
  if (paths)
    filePath = paths[0]
  else
    app.exit()
  return filePath
}

function closeAfterConfirmationDialog() {
  return electron.dialog.showMessageBox({
    type: "warning",
    buttons: ["Yes", "No"],
    defaultId: 0,
    title: "HaskellDO - Warning",
    message: "The current version of HaskellDO will overwrite the contents of the Main.hs file, do you want to continue?",
    cancelId : 1
  })
}

function startBackend(path){
  var os = require('os').platform()
  var corePath = ""
  var separator = ""
  if (os === "win32") {
    corePath = appRoot + "\\dist\\bin\\haskelldo-core-w64.exe"
    separator = "\\"
  } else if (os === "darwin") {
    corePath = appRoot + "/dist/bin/haskelldo-core-darwin"
    separator = "/"
  } else {
    corePath = appRoot + "/dist/bin/haskelldo-core-linux"
    separator = "/"
  }
  coreProcess = spawn("cd " + path + " && " + corePath + " \"" + path + separator + "Main.hs\"")
  setTimeout(function(){}, 3000)
  return coreProcess
}

function initApplication () {
  if (!stackIsOnPath()) {
    showStackNotOnPathError();
  }

  var filePath = openFileOrDie()
  var backendProcess

  if (closeAfterConfirmationDialog())
    app.exit(-1)

  if (!devModeActivated()) {
    backendProcess = startBackend(filePath)
  }

  const {width, height} = electron.screen.getPrimaryDisplay().workAreaSize
  mainWindow = new BrowserWindow({width, height, show: false})
  mainWindow.loadURL(`file://${ __dirname }/index.html`)
  if (devModeActivated())
    mainWindow.webContents.openDevTools()

  mainWindow.once('ready-to-show', () => {
    mainWindow.show()
  })

  mainWindow.on('closed', function () {
    if (os.platform() == 'win32') {
      child_process.exec('taskkill /pid ' + backendProcess.pid + ' /T /F')
    }
    electron.dialog.showMessageBox({ title: "HaskellDO quitting", message: "Remember to kill the haskelldo process"})
    mainWindow = null
  })

}
function stackIsOnPath () {
  try {
    var out = syncspawn('stack --help');
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
