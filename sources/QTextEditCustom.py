from PySide import QtCore, QtGui
from rpy2 import rinterface, robjects
import sys


class QTextEditCustom(QtCore.QObject):
    def __init__(self, ui):
        QtCore.QObject.__init__(self)
        self.ui = ui



    def eventFilter(self, widget, event):
        if event.type() == QtCore.QEvent.KeyPress:
            key = event.key()
            if key == QtCore.Qt.Key_Return:
                print('return')
                self.insertCommand()
                return True
        return QtGui.QWidget.eventFilter(self, widget, event)



    # Allow insert commands in a QTextEdit
    def insertCommand(self):
        comando = self.ui.edit_comandos.toPlainText()
        self.ui.text_result.append("> " + comando)

        def f(x):
            self.ui.text_result.textCursor().insertText(x)
        
        rinterface.set_writeconsole(f)
        backupList = robjects.globalenv.keys()
        resultado = robjects.r(comando)
        currentList = robjects.globalenv.keys()
        
        if len(backupList) == len(currentList):
            self.ui.text_result.append(str(resultado))

        rinterface.set_writeconsole(rinterface.consolePrint)