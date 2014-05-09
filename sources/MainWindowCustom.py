from PySide import QtCore, QtGui
from rpy2 import rinterface, robjects
import sys

class MainWindowCustom():

	#Constructor
	def __init__(self, ui):
		self.ui = ui

	def onCreate(self):
		# actions
		self.act_abrir = QtGui.QAction(QtGui.QIcon('./resources/openIcon.png'), 'Abrir archivo', self.ui)
		self.act_guardar = QtGui.QAction(QtGui.QIcon('./resources/saveIcon.png'), "Guardar Archivo", self.ui)
		self.act_cerrar = QtGui.QAction("Cerrar", self.ui)

		# # toolBar
		self.ui.toolBar.addAction(self.act_abrir)
		self.ui.toolBar.addAction(self.act_guardar)

		self.ui.mnu_archivo.addAction(self.act_abrir)
		self.ui.mnu_archivo.addAction(self.act_guardar)
		self.ui.mnu_archivo.addSeparator()
		self.ui.mnu_archivo.addAction(self.act_cerrar)

		# # signals
		QtCore.QObject.connect(self.act_abrir, QtCore.SIGNAL("triggered()"), self.abrirArchivo)
		QtCore.QObject.connect(self.ui.button_ejecutar, QtCore.SIGNAL("clicked()"), self.insertCommand)
		QtCore.QObject.connect(self.act_cerrar, QtCore.SIGNAL("triggered()"), self.ui.close)

		# init methods
		self.initToolbox()
		self.initTable()


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



	def abrirArchivo(self):
		fileName, _ = QtGui.QFileDialog.getOpenFileName(self.ui.mnu_archivo, "Abrir archivo", "./samples", "Archivos de datos (*.txt *.xls *.sav *.Rdata)")
		comando = "datos = lee.archivo.fnc('" + fileName + "')"

		backupList = robjects.globalenv.keys()
		resultado = robjects.r(comando)
		currentList = robjects.globalenv.keys()

		# Call a completeTable method
		if ".txt" in fileName:
			self.completeTableTxt()
		else:
			self.completeTableOthers()



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


	
	# Complete QTableWidget with a txt file
	def completeTableTxt(self):
		nrow = robjects.r("nrow(datos)")
		ncol = robjects.r("ncol(datos)")
		print("row " + str(nrow[0]) + " col " + str(ncol))

		self.ui.tableWidget.clear()
		
		self.ui.tableWidget.setRowCount(nrow[0])
		self.ui.tableWidget.setColumnCount(ncol[0])

		for i in range(ncol[0]):
			ind = i+1
			nameCol = robjects.r("colnames(datos["+str(ind)+"])")
			print str(ind) + "-" + str(nameCol[0])
			newItem = QtGui.QTableWidgetItem(str(nameCol[0]))
			self.ui.tableWidget.setHorizontalHeaderItem(i, newItem)

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
				self.ui.tableWidget.setItem(i, j, itemContent)



	# Complete QTableWidget with a sav, rdata or xls file
	def completeTableOthers(self):
		nrow = robjects.r("nrow(datos)")
		ncol = robjects.r("ncol(datos)")
		print("row " + str(nrow[0]) + " col " + str(ncol))

		self.ui.tableWidget.clear()
		
		self.ui.tableWidget.setRowCount(nrow[0])
		self.ui.tableWidget.setColumnCount(ncol[0])

		for i in range(ncol[0]):
			ind = i+1
			nameCol = robjects.r("colnames(datos["+str(ind)+"])")
			print str(ind) + "-" + str(nameCol[0])
			newItem = QtGui.QTableWidgetItem(str(nameCol[0]))
			self.ui.tableWidget.setHorizontalHeaderItem(i, newItem)
			self.insertVariables(nameCol,i)

		for i in range(nrow[0]):
			for j in range(ncol[0]):
				ni = i+1
				nj = j+1
				comando = "datos[" + str(ni) + "," + str(nj) + "]"
				content = robjects.r(comando)
				if hasattr(content, 'levels'):
					itemContent = QtGui.QTableWidgetItem(str(content.levels[0]))
				else:
					itemContent = QtGui.QTableWidgetItem(str(content[0]))

				self.ui.tableWidget.setItem(i, j, itemContent)



	# Insert variables into a listWidget
	def insertVariables(self, nameVar, i):
		self.ui.list_variables.insertItem(i, str(nameVar[0]))


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

