from PySide import QtCore, QtGui
from rpy2 import rinterface, robjects
import sys

class FncDatosCrear():

	#Constructor
	def __init__(self, ui, d_multiselector, d_crearVar_0, d_crearVar_1, d_crearVar_2):
		self.ui = ui
		self.d_multiselector = d_multiselector
		self.d_crearVar_0 = d_crearVar_0
		self.d_crearVar_1 = d_crearVar_1
		self.d_crearVar_2 = d_crearVar_2

		QtCore.QObject.connect(self.ui.act_newvar_1, QtCore.SIGNAL("triggered()"), self.openNewVar1)
		QtCore.QObject.connect(self.ui.act_newvar_2, QtCore.SIGNAL("triggered()"), self.openNewVar2)
		QtCore.QObject.connect(self.ui.act_newvar_3, QtCore.SIGNAL("triggered()"), self.openNewVar3)


	######################################################################################################
	### CREAR VARIABLE 1.
	### Crea la variable mediante variables existentes

	def openNewVar1(self):
		self.dialogUi = self.d_crearVar_0
		self.dialogUi.setWindowTitle("Crear nueva variable mediante variables existentes")
		self.dialogUi.show()
		self.dialogUi.var_select_1.clear()
		QtCore.QObject.connect(self.dialogUi.pushMultiSelector, QtCore.SIGNAL("clicked()"), self.openMultiSelector, QtCore.Qt.UniqueConnection)
		QtCore.QObject.connect(self.dialogUi.btn_add_nivel, QtCore.SIGNAL("clicked()"), self.addNewNivel, QtCore.Qt.UniqueConnection)

		QtCore.QObject.connect(self.dialogUi.buttonBox, QtCore.SIGNAL("accepted()"), self.acceptOpenNewVar1, QtCore.Qt.UniqueConnection)
		QtCore.QObject.connect(self.dialogUi.buttonBox, QtCore.SIGNAL("rejected()"), self.cancel, QtCore.Qt.UniqueConnection)




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


	######################################################################################################
	### CREAR VARIABLE 2.
	### Crea la variable mediante funciones de resumen

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




	######################################################################################################
	### CREAR VARIABLE 3.
	### Crea la variable a partir de un algoritmo dado
	def openNewVar3(self):
		self.dialogUi = self.d_crearVar_2
		self.dialogUi.setWindowTitle("Crear nueva variable a partir de un algoritmo dado")
		self.dialogUi.show()

		QtCore.QObject.connect(self.dialogUi.buttonBox, QtCore.SIGNAL("accepted()"), self.acceptOpenNewVar3, QtCore.Qt.UniqueConnection)
		QtCore.QObject.connect(self.dialogUi.buttonBox, QtCore.SIGNAL("rejected()"), self.cancel, QtCore.Qt.UniqueConnection)




	def acceptOpenNewVar3(self):
		fncAlgoritmica = self.dialogUi.fnc_algoritmica.toPlainText()
		def f(x):
			print x
		rinterface.set_writeconsole(f)

		comandR1 = "mi.expresion=" + fncAlgoritmica
		print comandR1 
		self.ui.text_result.append("> " + comandR1)
		comandR1 = robjects.r(comandR1)

		comandR2 = "iqitems = compute.fnc(datos, expresion=mi.expresion)"
		print comandR2
		self.ui.text_result.append("> " + comandR2)
		comandR2 = robjects.r(comandR2)

		self.ui.text_result.append(str(comandR2))
		rinterface.set_writeconsole(rinterface.consolePrint)




	def cancel(self):
		print ("*Cancel*")


