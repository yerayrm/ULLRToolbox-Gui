from PySide import QtCore, QtGui
from PySide.QtUiTools import QUiLoader
from PySide.QtGui import QApplication, QLineEdit
from sources.MainWindowCustom import MainWindowCustom
import sys

import resource_rc
resource_rc.qInitResources()


def main(argv=None):
    app = QApplication(sys.argv)
    loader = QUiLoader()
    ui = loader.load(':/mainwindow')
    mw = MainWindowCustom(ui)
    mw.onCreate()
    ui.setWindowTitle("ULLRToolbox")
    ui.setWindowIcon(QtGui.QIcon(':/logo64'))
    ui.showMaximized()
    return app.exec_()


if __name__ == '__main__':
    main()