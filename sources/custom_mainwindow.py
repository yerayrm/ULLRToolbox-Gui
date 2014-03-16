from PySide import QtCore, QtGui
from rpy2 import rinterface, robjects
import sys
import interface.mainwindow as mw

class CustomMainWindow(mw.Ui_MainWindow):
	def setupUi(self, MainWindow):
		super(CustomMainWindow, self).setupUi(MainWindow)

		QtCore.QObject.connect(self.button_ejecutar, QtCore.SIGNAL("clicked()"), self.insertCommand)
		# call a method to translate interface
		self.retranslateUi(MainWindow)


	def retranslateUi(self, MainWindow):
		super(CustomMainWindow, self).retranslateUi(MainWindow)
	

	def insertVariables(self, MainWindow):
		newItem = QtGui.QListWidgetItem()
		self.list_variables.insertItem(5, newItem)

	def insertCommand(self):
		comando = self.edit_comandos.toPlainText()
		self.text_result.append("> " + comando)

		def f(x):
			self.text_result.append(x)
		
		rinterface.set_writeconsole(f)
		resultado = robjects.r('print('+comando+')')
		self.text_result.append(resultado)
		rinterface.set_writeconsole(rinterface.consolePrint)

	
