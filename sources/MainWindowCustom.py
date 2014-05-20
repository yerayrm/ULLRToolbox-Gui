from PySide import QtCore, QtGui
from rpy2 import rinterface, robjects
import sys

class MainWindowCustom():

	#Constructor
	def __init__(self, ui, d_agregado, d_segmentado, d_crearVar_0, d_multiselector, d_crearVar_1):
		self.ui = ui
		self.d_agregado = d_agregado
		self.d_segmentado = d_segmentado
		self.d_crearVar_0 = d_crearVar_0
		self.d_multiselector = d_multiselector
		self.d_crearVar_1 = d_crearVar_1


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
		QtCore.QObject.connect(self.ui.act_newvar_1, QtCore.SIGNAL("triggered()"), self.openNewVar1)
		QtCore.QObject.connect(self.ui.act_newvar_2, QtCore.SIGNAL("triggered()"), self.openNewVar2)

		# signals main window
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



	def openNewVar2(self):
		self.dialogUi = self.d_crearVar_1
		self.dialogUi.setWindowTitle("Crear nueva variable mediante funciones de resumen")
		self.dialogUi.show()

		QtCore.QObject.connect(self.dialogUi.btn_add_factor, QtCore.SIGNAL("clicked()"), self.addNewFactor, QtCore.Qt.UniqueConnection)
		self.dialogUi.factor_table.cellClicked.connect(self.cellClickedTable)

		QtCore.QObject.connect(self.dialogUi.buttonBox, QtCore.SIGNAL("accepted()"), self.acceptOpenNewVar2, QtCore.Qt.UniqueConnection)
		QtCore.QObject.connect(self.dialogUi.buttonBox, QtCore.SIGNAL("rejected()"), self.cancel, QtCore.Qt.UniqueConnection)


	def acceptOpenNewVar2(self):
		comandR1 = "mi.lista=list("
		for i in range(self.dialogUi.factor_table.rowCount()):
			itemName = self.dialogUi.factor_table.item(i, 0).text()
			itemParams = self.dialogUi.factor_table.item(i, 1).text()
			print ("itemName: " + itemName)
			print ("itemParams" + itemParams)
			comandR1 = comandR1 + itemName + "=c(" + itemParams + ")"
			if (i != self.dialogUi.factor_table.rowCount()-1):
				comandR1 = comandR1 + ", "

		comandR1 = comandR1 + ")"

		if self.dialogUi.nv_media.isChecked():
			print "media"
			comandR2 = "iqitems=compute.fnc(datos, variables=mi.lista)"
		elif self.dialogUi.nv_mediana.isChecked():
			print "mediana"
			comandR2 = "iqitems=compute.fnc(datos, variables=mi.lista, estadistico='mediana')"
		elif self.dialogUi.nv_suma.isChecked():
			print "suma"
			comandR2 = "iqitems=compute.fnc(datos, variables=mi.lista, estadistico='suma')"
		elif self.dialogUi.nv_sc.isChecked():
			print "sc"
			comandR2 = "iqitems=compute.fnc(datos, variables=mi.lista, estadistico='sc')"
		else:
			self.openNewVar2()
		
		def f(x):
			print x
		rinterface.set_writeconsole(f)

		print comandR1
		self.ui.text_result.append("> " + comandR1)
		comandR1 = robjects.r(comandR1)

		print comandR2
		self.ui.text_result.append("> " + comandR2)
		comandR2 = robjects.r(comandR2)

		self.ui.text_result.append(str(comandR2))
		rinterface.set_writeconsole(rinterface.consolePrint)



	def acceptOpenNewVar1(self):
		nameNewVar = self.dialogUi.new_var_1.text()
		initNewVar = self.dialogUi.init_var.text()

		def f(x):
			print x
		rinterface.set_writeconsole(f)

		for i in range(self.dialogUi.nivel_table.rowCount()):
			comandR1 = ""
			comandR2 = "datos$" + nameNewVar + "='" + initNewVar + "'"
			comandR3 = "datos["
			for j in range(self.dialogUi.nivel_table.columnCount()-1):
				varCond = self.listMulti[j]
				comandR1 = "datos$" + varCond + "=recode(datos$" + varCond + ", \"NA=-999\")"
				print comandR1
				self.ui.text_result.append("> " + comandR1)
				comandR1 = robjects.r(comandR1)
				self.ui.text_result.append(str(comandR1))
				
				itemContent = self.dialogUi.nivel_table.item(i, j+1).text()
				comandR3 = comandR3 + "datos$" + itemContent
				if (self.dialogUi.nivel_table.rowCount()-1 == j):
					comandR3 = comandR3 + " & "
				else:reomandR3 + ", "
			nameCond = self.dialogUi.nivel_table.item(i, 0).text()
			comandR3 = comandR3 + "]$" + nameNewVar + "='" + nameCond + "'"

			print comandR2
			self.ui.text_result.append("> " + comandR2)
			comandR2 = robjects.r(comandR2)
			self.ui.text_result.append(str(comandR2))

			print comandR3
			self.ui.text_result.append("> " + comandR3)
			comandR3 = robjects.r(comandR3)
			self.ui.text_result.append(str(comandR3))

		rinterface.set_writeconsole(rinterface.consolePrint)



	def addNewFactor(self):
		row = self.dialogUi.factor_table.rowCount()
		self.dialogUi.factor_table.insertRow(row)




	def cellClickedTable(self, row, col):
		print("Row %d and Column %d was clicked" % (row, col))
		if (col == 1):
			self.row = row
			self.col = col
			self.openMultiSelector2()




	def openMultiSelector2(self):
		self.dialogMS = self.d_multiselector
		self.dialogMS.setWindowTitle("Selecciona los parametros")
		self.dialogMS.show()
		n_items = "length(names(datos))"
		n_items = robjects.r(n_items)
		n_items = n_items[0]

		self.dialogMS.selector_left.clear()
		self.dialogMS.selector_right.clear()
		for i in range(n_items):
			item_factor = "names(datos)[" + str(i+1) + "]"
			item_factor = robjects.r(item_factor)
			self.dialogMS.selector_left.insertItem(i, str(i+1) + " - " +  str(item_factor[0]))
			#iqitems$iq1=recode(iqitems$iq1, " NA=0 ")
			comandRecode = "datos$" + str(item_factor[0]) + "=recode(datos$" + str(item_factor[0]) + ", \"NA=0\")"
			comandRecode = robjects.r(comandRecode)

		QtCore.QObject.connect(self.dialogMS.moveToLeft, QtCore.SIGNAL("clicked()"), self.moveToLeft, QtCore.Qt.UniqueConnection)
		QtCore.QObject.connect(self.dialogMS.moveToRight, QtCore.SIGNAL("clicked()"), self.moveToRight, QtCore.Qt.UniqueConnection)

		QtCore.QObject.connect(self.dialogMS.buttonBox, QtCore.SIGNAL("accepted()"), self.acceptMultiSelector2, QtCore.Qt.UniqueConnection)
		QtCore.QObject.connect(self.dialogMS.buttonBox, QtCore.SIGNAL("rejected()"), self.cancel, QtCore.Qt.UniqueConnection)



	def acceptMultiSelector2(self):
		print "acept 2"
		self.listMulti = []
		
		for i in range(self.dialogMS.selector_right.count()):
			print "entro"
			item = self.dialogMS.selector_right.item(i).text()
			item = item[0]
			print item
			self.listMulti.append(item)

		itemContent = QtGui.QTableWidgetItem(', '.join(self.listMulti))
		self.dialogUi.factor_table.setItem(self.row, self.col, itemContent)
		print "lista: " + ', '.join(self.listMulti)















	def openNewVar1(self):
		self.dialogUi = self.d_crearVar_0
		self.dialogUi.setWindowTitle("Crear nueva variable mediante variables existentes")
		self.dialogUi.show()
		self.dialogUi.var_select_1.clear()
		QtCore.QObject.connect(self.dialogUi.pushMultiSelector, QtCore.SIGNAL("clicked()"), self.openMultiSelector, QtCore.Qt.UniqueConnection)
		QtCore.QObject.connect(self.dialogUi.btn_add_nivel, QtCore.SIGNAL("clicked()"), self.addNewNivel, QtCore.Qt.UniqueConnection)

		QtCore.QObject.connect(self.dialogUi.buttonBox, QtCore.SIGNAL("accepted()"), self.acceptOpenNewVar1, QtCore.Qt.UniqueConnection)
		QtCore.QObject.connect(self.dialogUi.buttonBox, QtCore.SIGNAL("rejected()"), self.cancel, QtCore.Qt.UniqueConnection)



	



	def addNewNivel(self):
		self.dialogUi.nivel_table.insertRow(self.dialogUi.nivel_table.rowCount())



	def openMultiSelector(self):
		self.dialogMS = self.d_multiselector
		self.dialogMS.setWindowTitle("Selecciona los parametros")
		self.dialogMS.show()
		n_items = "length(names(datos))"
		n_items = robjects.r(n_items)
		n_items = n_items[0]

		self.dialogMS.selector_left.clear()
		self.dialogMS.selector_right.clear()
		for i in range(n_items):
			item_factor = "names(datos)[" + str(i+1) + "]"
			item_factor = robjects.r(item_factor)
			self.dialogMS.selector_left.insertItem(i, str(item_factor[0]))

		QtCore.QObject.connect(self.dialogMS.moveToLeft, QtCore.SIGNAL("clicked()"), self.moveToLeft, QtCore.Qt.UniqueConnection)
		QtCore.QObject.connect(self.dialogMS.moveToRight, QtCore.SIGNAL("clicked()"), self.moveToRight, QtCore.Qt.UniqueConnection)

		QtCore.QObject.connect(self.dialogMS.buttonBox, QtCore.SIGNAL("accepted()"), self.acceptMultiSelector, QtCore.Qt.UniqueConnection)
		QtCore.QObject.connect(self.dialogMS.buttonBox, QtCore.SIGNAL("rejected()"), self.cancel, QtCore.Qt.UniqueConnection)



	def acceptMultiSelector(self):
		self.listMulti = []
		self.dialogUi.nivel_table.clear()
		for k in range(self.dialogUi.nivel_table.rowCount()):
			self.dialogUi.nivel_table.removeRow(0)

		for j in range(self.dialogUi.nivel_table.columnCount()+1):
			self.dialogUi.nivel_table.removeColumn(0)

		if (self.dialogMS.selector_right.count() > 0):
			self.dialogUi.nivel_table.insertColumn(0)
			newItem = QtGui.QTableWidgetItem("Nombre Valor")
			self.dialogUi.nivel_table.setHorizontalHeaderItem(0, newItem)
			for i in range(self.dialogMS.selector_right.count()):
				self.dialogUi.nivel_table.insertColumn(i+1)
				self.listMulti.append(self.dialogMS.selector_right.item(i).text())
				header = self.dialogMS.selector_right.item(i).text()
				newItem = QtGui.QTableWidgetItem(header)
				self.dialogUi.nivel_table.setHorizontalHeaderItem(i+1, newItem)

		self.dialogUi.var_select_1.clear()
		self.dialogUi.var_select_1.insert(', '.join(self.listMulti))
		print ', '.join(self.listMulti)



	def moveToRight(self):
		self.dialogMS.selector_right.addItem(str(self.dialogMS.selector_left.currentItem().text()))
		self.dialogMS.selector_left.takeItem(self.dialogMS.selector_left.currentRow())
		print "right n=" + str(self.dialogMS.selector_right.count())
		print "left n=" + str(self.dialogMS.selector_left.count())


	def moveToLeft(self):
		self.dialogMS.selector_left.addItem(str(self.dialogMS.selector_right.currentItem().text()))
		self.dialogMS.selector_right.takeItem(self.dialogMS.selector_right.currentRow())
		print "right n=" + str(self.dialogMS.selector_right.count())
		print "left n=" + str(self.dialogMS.selector_left.count())


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

