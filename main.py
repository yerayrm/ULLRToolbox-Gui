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

    # loader del mainwindows y todos los dialogs
    ui = loader.load(':/mainwindow')
    d_agregado = loader.load(':/agregado_dialog')
    d_segmentado = loader.load(':/segmentado_dialog')

    # instancio el mainwindow
    mw = MainWindowCustom(ui, d_agregado, d_segmentado)
    mw.onCreate()

    # inicio el programa
    ui.setWindowTitle("ULLRToolbox")
    ui.setWindowIcon(QtGui.QIcon(':/logo64'))
    ui.showMaximized()
    return app.exec_()


if __name__ == '__main__':
    main()