from PySide import QtCore, QtGui
from rpy2 import rinterface, robjects
import sys
import interface.mainwindow as mw

class CustomMainWindow(mw.Ui_MainWindow):
	def setupUi(self, MainWindow):
		super(CustomMainWindow, self).setupUi(MainWindow)

		# actions
		self.act_abrir = QtGui.QAction(QtGui.QIcon('./resources/openIcon.png'), 'open', MainWindow)
		self.act_guardar = QtGui.QAction(QtGui.QIcon('./resources/saveIcon.png'), "save", MainWindow)
		self.act_cerrar = QtGui.QAction(MainWindow)

		# toolBar
		self.toolBar.addAction(self.act_abrir)
		self.toolBar.addAction(self.act_guardar)

		self.mnu_archivo.addAction(self.act_abrir)
		self.mnu_archivo.addAction(self.act_guardar)
		self.mnu_archivo.addSeparator()
		self.mnu_archivo.addAction(self.act_cerrar)

		# signals
		QtCore.QObject.connect(self.act_abrir, QtCore.SIGNAL("triggered()"), self.abrirArchivo)
		QtCore.QObject.connect(self.button_ejecutar, QtCore.SIGNAL("clicked()"), self.insertCommand)
		QtCore.QObject.connect(self.act_cerrar, QtCore.SIGNAL("triggered()"), MainWindow.close)

		# init methods
		self.initToolbox()

		# call a method to translate interface
		self.retranslateUi(MainWindow)


	def retranslateUi(self, MainWindow):
		super(CustomMainWindow, self).retranslateUi(MainWindow)
	

	def initToolbox(self):
		def f(x):
			self.text_result.textCursor().insertText(x)
		
		rinterface.set_writeconsole(f)
		backupList = robjects.globalenv.keys()
		resultado = robjects.r("source('./script/ULLRtoolbox.v.1.0.R')")
		currentList = robjects.globalenv.keys()
		
		if len(backupList) == len(currentList):
			self.text_result.append(str(resultado))

		rinterface.set_writeconsole(rinterface.consolePrint)


	def insertVariables(self, MainWindow):
		newItem = QtGui.QListWidgetItem()
		self.list_variables.insertItem(5, newItem)


	def abrirArchivo(self):
		fileName, _ = QtGui.QFileDialog.getOpenFileName(self.mnu_archivo, "Abrir archivo", "./samples", "Archivos de datos (*.txt *.xls *.sav *.Rdata)")
		self.text_result.append("[FICHERO]" + fileName)

		comando = "datos = lee.archivo.fnc('" + fileName + "')"
		self.text_result.append("> " + comando)

		def f(x):
			self.text_result.textCursor().insertText(x)
		
		rinterface.set_writeconsole(f)
		backupList = robjects.globalenv.keys()
		resultado = robjects.r(comando)
		currentList = robjects.globalenv.keys()
		
		if len(backupList) == len(currentList):
			self.text_result.append(str(resultado))

		rinterface.set_writeconsole(rinterface.consolePrint)



	def insertCommand(self):
		comando = self.edit_comandos.toPlainText()
		self.text_result.append("> " + comando)

		def f(x):
			self.text_result.textCursor().insertText(x)
		
		rinterface.set_writeconsole(f)
		backupList = robjects.globalenv.keys()
		resultado = robjects.r(comando)
		currentList = robjects.globalenv.keys()
		
		if len(backupList) == len(currentList):
			self.text_result.append(str(resultado))

		rinterface.set_writeconsole(rinterface.consolePrint)

	
