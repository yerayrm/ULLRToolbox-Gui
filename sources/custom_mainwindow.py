from PySide import QtCore, QtGui
from rpy2 import rinterface, robjects
import sys
import interface.mainwindow as mw

class CustomMainWindow(mw.Ui_MainWindow):
	def setupUi(self, MainWindow):
		super(CustomMainWindow, self).setupUi(MainWindow)

		# actions
		self.act_abrir = QtGui.QAction(QtGui.QIcon('./resources/openIcon.png'), "open", MainWindow)
		self.act_guardar = QtGui.QAction(QtGui.QIcon('./resources/saveIcon.png'), "save", MainWindow)

		# toolBar
		self.toolBar.addAction(self.act_abrir)
		self.toolBar.addAction(self.act_guardar)

		# signals
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
		self.text_result.append("")

		def f(x):
			#self.text_result.append(x)
			self.text_result.textCursor().insertText(x)
		
		rinterface.set_writeconsole(f)
		resultado = robjects.r('print('+comando+')')
		self.text_result.append(resultado)
		rinterface.set_writeconsole(rinterface.consolePrint)

	
