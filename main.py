from PySide import QtCore, QtGui
import sys
import sources.custom_mainwindow as mw

class ControlMainWindow(QtGui.QMainWindow):
    def __init__(self, parent=None):
        super(ControlMainWindow, self).__init__(parent)
        self.ui = mw.CustomMainWindow()
        self.ui.setupUi(self)

if __name__ == "__main__":
    app = QtGui.QApplication(sys.argv)
    mySW = ControlMainWindow()
    mySW.showMaximized()
    sys.exit(app.exec_())