from PySide import QtCore, QtGui
from rpy2 import rinterface, robjects
import sys

class MainWindowCustom():

	#Constructor
	def __init__(self, ui, d_agregado, d_segmentado):
		self.ui = ui
		self.d_agregado = d_agregado
		self.d_segmentado = d_segmentado


	def onCreate(self):
		# actions
		self.act_abrir = QtGui.QAction(QtGui.QIcon('./resources/openIcon.png'), 'Abrir archivo', self.ui)
		self.act_guardar = QtGui.QAction(QtGui.QIcon('./resources/saveIcon.png'), "Guardar Archivo", self.ui)
		self.act_cerrar = QtGui.QAction("Cerrar", self.ui)

		# toolBar
		self.ui.toolBar.addAction(self.act_abrir)
		self.ui.toolBar.addAction(self.act_guardar)

		# Archivo
		self.ui.mnu_archivo.addAction(self.act_abrir)
		self.ui.mnu_archivo.addAction(self.act_guardar)
		self.ui.mnu_archivo.addSeparator()
		self.ui.mnu_archivo.addAction(self.act_cerrar)


		# signals
		##Archivo
		QtCore.QObject.connect(self.act_abrir, QtCore.SIGNAL("triggered()"), self.abrirArchivo)
		QtCore.QObject.connect(self.act_cerrar, QtCore.SIGNAL("triggered()"), self.ui.close)
		##Datos
		QtCore.QObject.connect(self.ui.act_agregado, QtCore.SIGNAL("triggered()"), self.openAgregadoDialog)
		QtCore.QObject.connect(self.ui.act_segmentado, QtCore.SIGNAL("triggered()"), self.openSegmentadoDialog)

		QtCore.QObject.connect(self.ui.button_ejecutar, QtCore.SIGNAL("clicked()"), self.insertCommand)

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


	def openAgregadoDialog(self):
		self.dialogUi = self.d_agregado
		self.dialogUi.setWindowTitle("Agregado")
		self.dialogUi.show()
		self.dialogUi.te_matriz.clear()
		self.dialogUi.te_matriz.append("list(A=c('a1','a2'), B=c('b1','b2'))")
		self.dialogUi.buttonBox.accepted.connect(self.acceptAgregado)
		self.dialogUi.buttonBox.rejected.connect(self.cancel)


	def openSegmentadoDialog(self):
		self.dSegmentadoUi = self.d_segmentado
		self.dSegmentadoUi.setWindowTitle("Segmentado")
		self.dSegmentadoUi.show()
		## cargo los elementos del combobox
		def f(x):
			print x
		rinterface.set_writeconsole(f)
		n_items = "length(names(datos))"
		n_items = robjects.r(n_items)
		n_items = n_items[0]
		self.dSegmentadoUi.factorCombo.clear()
		for i in range(n_items):
			item_factor = "names(datos)[" + str(i+1) + "]"
			item_factor = robjects.r(item_factor)
			self.dSegmentadoUi.factorCombo.addItem(str(item_factor[0]))

		rinterface.set_writeconsole(rinterface.consolePrint)
 
		## senales
		self.dSegmentadoUi.buttonBox.accepted.connect(self.acceptSegmentado)
		self.dSegmentadoUi.buttonBox.rejected.connect(self.cancel)



	def acceptSegmentado(self):
		print ("*Accept*")
		comando = "x.factor=divide.por.factor.fnc(datos, que.factor='" + str(self.dSegmentadoUi.factorCombo.currentText()) + "')"
		self.ui.text_result.append("> " + comando)
		def f(x):
			print x
		rinterface.set_writeconsole(f)
		resultado = robjects.r(comando)
		self.ui.text_result.append(str(resultado))
		rinterface.set_writeconsole(rinterface.consolePrint)



	def acceptAgregado(self):
		print "*Accept*"
		# matriz de los datos
		fac_intra = self.dialogUi.te_matriz.toPlainText()
		print str(fac_intra)
		# agregado por
		if self.dialogUi.rb_sujetos.isChecked():
			print "sujetos"
			agregado_por = "sujeto"
		elif self.dialogUi.rb_items.isChecked():
			print "items"
			agregado_por = "item"
		else:
			self.openAgregadoDialog()
		# ejecuto fac.intra, length, apila.datos, agrega.datos
		def f(x):
			print x

		rinterface.set_writeconsole(f)
		##cambia.nombre
		crea_nombre = "datos=crea.nombre.item.fnc(datos)"
		crea_nombre = robjects.r(crea_nombre)
		##length
		n_items = "length(names(datos))"
		n_items = robjects.r(n_items)
		n_items = n_items[0]
		##fac.intra
		fac_intra = "fac.intra=" + fac_intra
		fac_intra = robjects.r(fac_intra)
		##apila.los.datos
		apila_datos = "datos.ap=apila.los.datos.fnc(datos, fac.intra=fac.intra, col.empieza.item=1, n.item=" + str(n_items) + ")"
		apila_datos = robjects.r(apila_datos)
		##agrega.sujeto
		# estadisticos descriptivos
		if self.dialogUi.rb_dt.isChecked():
			print "dt"
			estadisticos = "dt"
			agrega_sujeto_str = "agrega.sujeto=agrega.los.datos.fnc(datos.ap, que.factor=c('" + agregado_por + "','A','B'), estadistico='dt')"
		elif self.dialogUi.rb_media.isChecked():
			print "media"
			estadisticos = "media"
			agrega_sujeto_str = "agrega.sujeto=agrega.los.datos.fnc(datos.ap, que.factor=c('" + agregado_por + "','A','B'))"
		elif self.dialogUi.rb_n.isChecked():
			print "n"
			estadisticos = "n"
			agrega_sujeto_str = "agrega.sujeto=agrega.los.datos.fnc(datos.ap, que.factor=c('" + agregado_por + "','A','B'), estadistico='n')"
		elif self.dialogUi.rb_suma.isChecked():
			print "suma"
			estadisticos = "suma"
			agrega_sujeto_str = "agrega.sujeto=agrega.los.datos.fnc(datos.ap, que.factor=c('" + agregado_por + "','A','B'), estadistico='suma')"
		else:
			self.openAgregadoDialog()
		agrega_sujeto = robjects.r(agrega_sujeto_str)

		self.ui.text_result.append("")
		self.ui.text_result.append("")
		self.ui.text_result.append("> " + agrega_sujeto_str)
		self.ui.text_result.append("")
		self.ui.text_result.append("****************** AGREGADO *****************************")
		self.ui.text_result.append("   -   Agregado por " + agregado_por)
		self.ui.text_result.append("   -   Estadisticos descriptivos: " + estadisticos)
		self.ui.text_result.append("*********************************************************")
		self.ui.text_result.append(str(agrega_sujeto))

		rinterface.set_writeconsole(rinterface.consolePrint)




	def cancel(self):
		print ("*Cancel*")



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

