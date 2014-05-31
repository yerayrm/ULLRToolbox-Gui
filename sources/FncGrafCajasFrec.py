from PySide import QtCore, QtGui
from rpy2 import rinterface, robjects
import sys
import time
import threading

class FncGrafCajasFrec():

	#Constructor
	def __init__(self, ui, d_cajas, d_frec, d_multiselector):
		self.ui = ui
		self.d_cajas = d_cajas
		self.d_frec = d_frec
		self.d_multiselector = d_multiselector
		
		QtCore.QObject.connect(self.ui.act_cajas, QtCore.SIGNAL("triggered()"), self.openCajas)
		QtCore.QObject.connect(self.ui.act_frecuencias, QtCore.SIGNAL("triggered()"), self.openFrecuencias)
		


	######################################################################################################
	### DIAGRAMA DE CAJAS
	def openCajas(self):
		self.dialogUi = self.d_cajas
		self.dialogUi.setWindowTitle("Diagrama de cajas")
		self.dialogUi.show()

		# Abro el multiselector
		self.dialogUi.var_select.clear()
		QtCore.QObject.connect(self.dialogUi.pushMultiSelector, QtCore.SIGNAL("clicked()"), self.openMultiSelector, QtCore.Qt.UniqueConnection)

		# Inicializo los combobox
		def f(x):
			print x
		rinterface.set_writeconsole(f)
		n_items = "length(names(datos))"
		n_items = robjects.r(n_items)
		n_items = n_items[0]
		self.dialogUi.cb_variable.clear()
		for i in range(n_items):
			item_factor = "names(datos)[" + str(i+1) + "]"
			item_factor = robjects.r(item_factor)
			self.dialogUi.cb_variable.addItem(str(item_factor[0]))

		rinterface.set_writeconsole(rinterface.consolePrint)

		# Signals
		QtCore.QObject.connect(self.dialogUi.buttonBox, QtCore.SIGNAL("accepted()"), self.acceptCajas, QtCore.Qt.UniqueConnection)
		QtCore.QObject.connect(self.dialogUi.buttonBox, QtCore.SIGNAL("rejected()"), self.cancel, QtCore.Qt.UniqueConnection)




	def acceptCajas(self):
		print ("*Accept*")

		variable = str(self.dialogUi.cb_variable.currentText())
		factor = ":".join(self.listMulti)

		if self.dialogUi.cb_casos.isChecked():
			comando = "diagrama.cajas.fnc(datos, vd='" + variable + "', que.factor='" + factor + "', identifica=T)"
		else:
			comando = "diagrama.cajas.fnc(datos, vd='" + variable + "', que.factor='" + factor + "')"


		self.ui.text_result.append("> " + comando)

		def f(x):
			self.ui.text_result.textCursor().insertText(x)

		rinterface.set_writeconsole(f)
		resultado = robjects.r(comando)
		self.ui.text_result.append(str(resultado))
		
		rinterface.set_writeconsole(rinterface.consolePrint)



	def cancel(self):
		print ("*Cancel*")



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



	def acceptMultiSelector(self):
		self.listMulti = []
		if (self.dialogMS.selector_right.count() > 0):
			for i in range(self.dialogMS.selector_right.count()):
				self.listMulti.append(self.dialogMS.selector_right.item(i).text())

		self.dialogUi.var_select.clear()
		self.dialogUi.var_select.insert(', '.join(self.listMulti))
		print ', '.join(self.listMulti)




	######################################################################################################
	### DIAGRAMA DE FRECUENCIAS
	def openFrecuencias(self):
		self.dialogUi = self.d_frec
		self.dialogUi.setWindowTitle("Frecuencias")
		self.dialogUi.show()
