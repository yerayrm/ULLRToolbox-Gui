from PySide import QtCore, QtGui
from rpy2 import rinterface, robjects
import sys

class FncArchivo():

	#Constructor
	def __init__(self, ui):
		self.ui = ui
		
		# signals
		QtCore.QObject.connect(self.ui.act_abrir_archivo, QtCore.SIGNAL("triggered()"), self.abrirArchivo)
		QtCore.QObject.connect(self.ui.act_guardar_archivo, QtCore.SIGNAL("triggered()"), self.guardarArchivo)
		QtCore.QObject.connect(self.ui.act_exportar_archivo, QtCore.SIGNAL("triggered()"), self.exportarArchivo)
		QtCore.QObject.connect(self.ui.act_Cerrar, QtCore.SIGNAL("triggered()"), self.ui.close)



	# Allow open a file
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



	# Save a file in RData format
	def guardarArchivo(self):
		fileName, _ = QtGui.QFileDialog.getSaveFileName(self.ui.mnu_archivo, "Guardar archivo", "", "Archivos de R (*.Rdata)")
		
		comando = "guarda.Rdata.fnc(datos, nombre='"+ fileName +"')"
		self.ui.text_result.append("> " + comando)

		def f(x):
			self.ui.text_result.textCursor().insertText(x)

		rinterface.set_writeconsole(f)
		resultado = robjects.r(comando)
		rinterface.set_writeconsole(rinterface.consolePrint)



	# Save a file in txt format
	def exportarArchivo(self):
		fileName, _ = QtGui.QFileDialog.getSaveFileName(self.ui.mnu_archivo, "Exortar archivo como txt", "", "Archivos de texto (*.txt)")
		
		comando = "exporta.txt.fnc(datos, nombre='"+ fileName +"')"
		self.ui.text_result.append("> " + comando)

		def f(x):
			self.ui.text_result.textCursor().insertText(x)

		rinterface.set_writeconsole(f)
		resultado = robjects.r(comando)
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

