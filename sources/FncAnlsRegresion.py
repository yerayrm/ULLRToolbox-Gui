#!/usr/bin/env python
# -*- coding: utf-8 -*-

from PySide import QtCore, QtGui
from rpy2 import rinterface, robjects
import sys

class FncAnlsRegresion():

	#Constructor
	def __init__(self, ui, d_regr_multiple, d_regr_mediacion, d_multiselector):
		self.ui = ui
		self.d_regr_multiple = d_regr_multiple
		self.d_regr_mediacion = d_regr_mediacion
		self.d_multiselector = d_multiselector

		QtCore.QObject.connect(self.ui.act_regr_multiple, QtCore.SIGNAL("triggered()"), self.openRegresionMultiple)
		QtCore.QObject.connect(self.ui.act_regr_mediacion, QtCore.SIGNAL("triggered()"), self.openMediacion)
		

	######################################################################################################
	### REGRESION MULTIPLE
	def openRegresionMultiple(self):
		self.dialogUi = self.d_regr_multiple
		self.dialogUi.setWindowTitle(("Regresión múltiple").decode("utf8"))
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
		self.dialogUi.cb_vd.clear()
		for i in range(n_items):
			item_factor = "names(datos)[" + str(i+1) + "]"
			item_factor = robjects.r(item_factor)
			self.dialogUi.cb_vd.addItem(str(item_factor[0]))

		rinterface.set_writeconsole(rinterface.consolePrint)

		# Signals
		QtCore.QObject.connect(self.dialogUi.buttonBox, QtCore.SIGNAL("accepted()"), self.acceptRegresion, QtCore.Qt.UniqueConnection)
		QtCore.QObject.connect(self.dialogUi.buttonBox, QtCore.SIGNAL("rejected()"), self.cancel, QtCore.Qt.UniqueConnection)



	def acceptRegresion(self):
		print ("*Accept*")

		#variables
		variables = "c('"
		variables = variables + "', '".join(self.listMulti)
		variables = variables + "')"

		#variable dependiente
		vd = str(self.dialogUi.cb_vd.currentText())

		#grafica
		if self.dialogUi.check_grafica.isChecked():
			grafica = ", grafica=T"
		else:
			grafica = ""

		#paso a paso
		if self.dialogUi.check_paso.isChecked():
			paso = ", paso.a.paso=T"
		else:
			paso = ""

		#robusta
		if self.dialogUi.check_robusta.isChecked():
			robusta = ", robusta=T"
		else:
			robusta = ""

		#dominancia
		if self.dialogUi.check_dominancia.isChecked():
			dominancia = ", dominancia=T"
		else:
			dominancia = ""

		#interaccion
		if self.dialogUi.check_interaccion.isChecked():
			interaccion = ", interaccion=T"
		else:
			interaccion = ""

		#comando
		comando = "regresion.multiple.fnc(datos, vd='" + vd + "', variables=" + variables + grafica + paso + robusta + dominancia + interaccion + ")"

		self.ui.text_result.append("> " + comando)

		def f(x):
			self.ui.text_result.textCursor().insertText(x.decode("utf8"))

		rinterface.set_writeconsole(f)
		resultado = robjects.r(comando)
		self.ui.text_result.append(str(resultado.decode("utf8")))
		
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
	### MEDIACION
	def openMediacion(self):
		self.dialogUi = self.d_regr_mediacion
		self.dialogUi.setWindowTitle(("Mediación").decode("utf8"))
		self.dialogUi.show()

		# Abro el multiselector
		self.listMulti = []
		self.dialogUi.var_select.clear()
		QtCore.QObject.connect(self.dialogUi.pushMultiSelector, QtCore.SIGNAL("clicked()"), self.openMultiSelector, QtCore.Qt.UniqueConnection)

		# Inicializo los combobox
		def f(x):
			print x
		rinterface.set_writeconsole(f)
		n_items = "length(names(datos))"
		n_items = robjects.r(n_items)
		n_items = n_items[0]
		self.dialogUi.cb_dependiente.clear()
		self.dialogUi.cb_independiente.clear()
		self.dialogUi.cb_moduladora.clear()
		for i in range(n_items):
			item_factor = "names(datos)[" + str(i+1) + "]"
			item_factor = robjects.r(item_factor)
			self.dialogUi.cb_dependiente.addItem(str(item_factor[0]))
			self.dialogUi.cb_independiente.addItem(str(item_factor[0]))
			self.dialogUi.cb_moduladora.addItem(str(item_factor[0]))

		rinterface.set_writeconsole(rinterface.consolePrint)

		# Signals
		QtCore.QObject.connect(self.dialogUi.buttonBox, QtCore.SIGNAL("accepted()"), self.acceptMediacion, QtCore.Qt.UniqueConnection)
		QtCore.QObject.connect(self.dialogUi.buttonBox, QtCore.SIGNAL("rejected()"), self.cancel, QtCore.Qt.UniqueConnection)




	def acceptMediacion(self):
		print ("*Accept*")

		vd = str(self.dialogUi.cb_dependiente.currentText())
		vi = str(self.dialogUi.cb_independiente.currentText())
		m = str(self.dialogUi.cb_moduladora.currentText())

		covariante = "c('"
		covariante = covariante + "', '".join(self.listMulti)
		covariante = covariante + "')"

		#tipo mediacion
		if self.dialogUi.rb_basic.isChecked():
			mediacion = "list(vd='" + vd + "',vi='" + vi + "',m='" + m + "')"
			comando = "analisis.mediacion.fnc(datos, " + mediacion + ")"
		elif self.dialogUi.rb_interac.isChecked():
			mediacion = "list(vd='" + vd + "',vi='" + vi + "',m='" + m + "')"
			comando = "analisis.mediacion.fnc(datos, " + mediacion + ", interaccion.vi.m=T)"
		elif self.dialogUi.rb_covariante.isChecked():
			mediacion = "list(vd='" + vd + "',vi='" + vi + "',m='" + m + "', covariante=" + covariante + ")"
			comando = "analisis.mediacion.fnc(datos, " + mediacion + ")"
		else:
			self.openMediacion()

		#ejecucion
		self.ui.text_result.append("> " + comando)

		def f(x):
			self.ui.text_result.textCursor().insertText(x.decode("utf8"))

		rinterface.set_writeconsole(f)
		resultado = robjects.r(comando)
		self.ui.text_result.append(str(resultado.decode("utf8")))
		
		rinterface.set_writeconsole(rinterface.consolePrint)

