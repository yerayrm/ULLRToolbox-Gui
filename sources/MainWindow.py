from PySide import QtCore, QtGui
from rpy2 import rinterface, robjects
from QTextEditCustom import QTextEditCustom
import sys

class MainWindow():

	# Constructor
	def __init__(self, ui):
		self.ui = ui

		# signals in main window
		QtCore.QObject.connect(self.ui.button_ejecutar, QtCore.SIGNAL("clicked()"), self.insertCommand)

		# capture key event
		self.keyPressEater = QTextEditCustom(self.ui)
		self.ui.edit_comandos.installEventFilter(self.keyPressEater)

		# init methods
		self.initToolbox()
		self.initTable()



	# Initialize the ULLRToolbox script
	def initToolbox(self):
		def f(x):
			self.ui.text_result.textCursor().insertText(x)
		
		rinterface.set_writeconsole(f)
		backupList = robjects.globalenv.keys()
		resultado = robjects.r("source('./script/ULLRtoolbox.v.1.0.R')")
		currentList = robjects.globalenv.keys()
		
		if len(backupList) == len(currentList):
			self.ui.text_result.append(str(resultado))

		rinterface.set_writeconsole(rinterface.consolePrint)

	

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



	# Initialize a QTableWidget when the program is open
	def initTable(self):
		nrow = 50
		ncol = 20
		
		self.ui.tableWidget.setRowCount(nrow)
		self.ui.tableWidget.setColumnCount(ncol)

		for i in range(ncol):
			ind = i+1
			header = "Var " + str(ind)
			newItem = QtGui.QTableWidgetItem(header)
			self.ui.tableWidget.setHorizontalHeaderItem(i, newItem)

