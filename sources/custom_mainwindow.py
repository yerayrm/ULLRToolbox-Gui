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

		# toolBar
		self.toolBar.addAction(self.act_abrir)
		self.toolBar.addAction(self.act_guardar)

		self.mnu_archivo.addAction(self.act_abrir)
		#self.menubar.addAction(self.mnu_archivo.menuAction())

		# signals
		#QtCore.QObject.connect(self.act_abrir, QtCore.SIGNAL("triggered()"), self.abrirArchivo())
		QtCore.QObject.connect(self.button_ejecutar, QtCore.SIGNAL("clicked()"), self.insertCommand)

		# call a method to translate interface
		self.retranslateUi(MainWindow)


	def retranslateUi(self, MainWindow):
		super(CustomMainWindow, self).retranslateUi(MainWindow)
	

	def insertVariables(self, MainWindow):
		newItem = QtGui.QListWidgetItem()
		self.list_variables.insertItem(5, newItem)


	def abrirArchivo(self):
		#fileName = QtGui.QFileDialog.getOpenFileName(self, tr("Open Image"), "/home/jana", tr("Image Files (*.png *.jpg *.bmp)"))
		path, filter = QtGui.QFileDialog.getOpenFileName(self, "Find Files")


	def insertCommand(self):
		comando = self.edit_comandos.toPlainText()
		self.text_result.append("> " + comando)
		#self.text_result.append("")

		def f(x):
			self.text_result.textCursor().insertText(x)
		
		rinterface.set_writeconsole(f)
		backupList = robjects.globalenv.keys()
		resultado = robjects.r(comando)
		currentList = robjects.globalenv.keys()
		
		if len(backupList) == len(currentList):
			self.text_result.append(str(resultado))

		rinterface.set_writeconsole(rinterface.consolePrint)

	
