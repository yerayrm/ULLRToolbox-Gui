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
    d_crearVar_0 = loader.load(':/crear_variable_existente')
    d_crearVar_1 = loader.load(':/crear_variable_funcion')
    d_crearVar_2 = loader.load(':/crear_variable_algoritmo')
    d_multiselector = loader.load(':/multiselect_column')

    # instancio el mainwindow
    mw = MainWindowCustom(ui, d_agregado, d_segmentado, d_crearVar_0, d_multiselector, d_crearVar_1, d_crearVar_2)
    mw.onCreate()

    # inicio el programa
    ui.setWindowTitle("ULLRToolbox")
    ui.setWindowIcon(QtGui.QIcon(':/logo64'))
    ui.showMaximized()
    return app.exec_()


if __name__ == '__main__':
    main()