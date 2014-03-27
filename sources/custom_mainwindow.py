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
		self.initTable()
		

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

		comando = "datos = lee.archivo.fnc('" + fileName + "')"

		#def f(x):
		#	self.text_result.textCursor().insertText(x)
		
		#rinterface.set_writeconsole(f)
		backupList = robjects.globalenv.keys()
		resultado = robjects.r(comando)
		currentList = robjects.globalenv.keys()

		#rinterface.set_writeconsole(rinterface.consolePrint)
		self.completeTable()



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

	

	def completeTable(self):
		nrow = robjects.r("nrow(datos)")
		ncol = robjects.r("ncol(datos)")
		print("row " + str(nrow[0]) + " col " + str(ncol))

		self.tableWidget.clear()
		
		self.tableWidget.setRowCount(nrow[0])
		self.tableWidget.setColumnCount(ncol[0])

		for i in range(ncol[0]):
			ind = i+1
			nameCol = robjects.r("colnames(datos["+str(ind)+"])")
			print str(ind) + "-" + str(nameCol[0])
			newItem = QtGui.QTableWidgetItem(str(nameCol[0]))
			self.tableWidget.setHorizontalHeaderItem(i, newItem)

		for i in range(nrow[0]):
			for j in range(ncol[0]):
				ni = i+1
				nj = j+1
				comando = "datos[" + str(ni) + "," + str(nj) + "]"
				#print comando
				content = robjects.r(comando)
				#print comando + " - " + str(content.levels[0])
				itemContent = QtGui.QTableWidgetItem(str(content.levels[0]))
				#itemContent = QtGui.QTableWidgetItem(str(content[0]))
				self.tableWidget.setItem(i, j, itemContent)



	def initTable(self):
		nrow = 50
		ncol = 20
		
		self.tableWidget.setRowCount(nrow)
		self.tableWidget.setColumnCount(ncol)

		for i in range(ncol):
			ind = i+1
			header = "Var " + str(ind)
			newItem = QtGui.QTableWidgetItem(header)
			self.tableWidget.setHorizontalHeaderItem(i, newItem)
