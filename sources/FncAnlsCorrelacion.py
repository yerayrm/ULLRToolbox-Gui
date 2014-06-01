#!/usr/bin/env python
# -*- coding: utf-8 -*-

from PySide import QtCore, QtGui
from rpy2 import rinterface, robjects
import sys

class FncAnlsCorrelacion():

	#Constructor
	def __init__(self, ui, d_correlacion, d_multiselector, d_multiselector_2):
		self.ui = ui
		self.d_correlacion = d_correlacion
		self.d_multiselector = d_multiselector
		self.d_multiselector_2 = d_multiselector_2

		QtCore.QObject.connect(self.ui.act_correlacion, QtCore.SIGNAL("triggered()"), self.openCorrelacion)
		


	def openCorrelacion(self):
		self.dialogUi = self.d_correlacion
		self.dialogUi.setWindowTitle("Correlación".decode("utf8"))
		self.dialogUi.show()

		self.listMulti_1 = []
		self.listMulti_2 = []

		# Oculto todo lo necesario
		self.dialogUi.groupBox_2.setHidden(True)
		self.dialogUi.groupBox_3.setHidden(True)
		self.dialogUi.groupBox_4.setHidden(True)
		self.dialogUi.groupBox_5.setHidden(True)
		self.dialogUi.groupBox_6.setHidden(True)
		self.dialogUi.setFixedHeight(150)

		# 
		QtCore.QObject.connect(self.dialogUi.rb_pearsson, QtCore.SIGNAL("clicked()"), self.onClickPearsson, QtCore.Qt.UniqueConnection)
		QtCore.QObject.connect(self.dialogUi.rb_policorica, QtCore.SIGNAL("clicked()"), self.onClickCorica, QtCore.Qt.UniqueConnection)
		QtCore.QObject.connect(self.dialogUi.rb_tetracorica, QtCore.SIGNAL("clicked()"), self.onClickCorica, QtCore.Qt.UniqueConnection)
		QtCore.QObject.connect(self.dialogUi.rb_parcial, QtCore.SIGNAL("clicked()"), self.onClickParcial, QtCore.Qt.UniqueConnection)
		QtCore.QObject.connect(self.dialogUi.rb_semiparcial, QtCore.SIGNAL("clicked()"), self.onClickParcial, QtCore.Qt.UniqueConnection)


		# Abro los multiselectores
		self.dialogUi.var_select_1.clear()
		QtCore.QObject.connect(self.dialogUi.pushMultiSelector_1, QtCore.SIGNAL("clicked()"), self.openMultiSelector_1, QtCore.Qt.UniqueConnection)

		self.dialogUi.var_select_2.clear()
		QtCore.QObject.connect(self.dialogUi.pushMultiSelector_2, QtCore.SIGNAL("clicked()"), self.openMultiSelector_2, QtCore.Qt.UniqueConnection)


		# Inicializo los combobox
		def f(x):
			print x
		rinterface.set_writeconsole(f)
		n_items = "length(names(datos))"
		n_items = robjects.r(n_items)
		n_items = n_items[0]
		self.dialogUi.cb_factor.clear()
		self.dialogUi.cb_factor.addItem(str("Sin factor"))
		for i in range(n_items):
			item_factor = "names(datos)[" + str(i+1) + "]"
			item_factor = robjects.r(item_factor)
			self.dialogUi.cb_factor.addItem(str(item_factor[0]))

		rinterface.set_writeconsole(rinterface.consolePrint)

		# Signals
		QtCore.QObject.connect(self.dialogUi.buttonBox, QtCore.SIGNAL("accepted()"), self.accept, QtCore.Qt.UniqueConnection)
		QtCore.QObject.connect(self.dialogUi.buttonBox, QtCore.SIGNAL("rejected()"), self.cancel, QtCore.Qt.UniqueConnection)



	def accept(self):
		print ("*Accept*")

		variables = "c('"
		variables = variables + "', '".join(self.listMulti_1)
		variables = variables + "')"

		factor = str(self.dialogUi.cb_factor.currentText())
		if (factor == "Sin factor"):
			factor = ""
		else:
			factor = ", que.factor='" + factor + "'"

		control = "c('"
		control = control + "', '".join(self.listMulti_2)
		control = control + "')"

		if self.dialogUi.rb_listwise.isChecked():
			caso_completo = ", caso.completo=T"
		elif self.dialogUi.rb_pairwise.isChecked():
			caso_completo = ", caso.completo=F"
		else:
			caso_completo = ""

		if self.dialogUi.cb_contraste.isChecked():
			contraste = ", contraste=T"
		else:
			contraste = ""

		if self.dialogUi.cb_covarianza.isChecked():
			covarianza = ", covarianza=T"
		else:
			covarianza = ""


		# Pearsson
		if self.dialogUi.rb_pearsson.isChecked():
			comando = "correlacion.fnc(datos, variables=" + variables + factor + covarianza + contraste + caso_completo + ")"
		
		# Policorica
		elif self.dialogUi.rb_policorica.isChecked():
			comando = "correlacion.fnc(datos, variables=" + variables + ", tipo='policorica')"

		# Tetracorica
		elif self.dialogUi.rb_tetracorica.isChecked():
			comando = "correlacion.fnc(datos, variables=" + variables + ", tipo='tetracorica')"

		# Parcial
		elif self.dialogUi.rb_parcial.isChecked():
			comando = "correlacion.fnc(datos, variables=" + variables + ", parcial=T, control=" + control + ")"		

		# Semiparcial
		elif self.dialogUi.rb_semiparcial.isChecked():
			comando = "correlacion.fnc(datos, variables=" + variables + ", sparcial=T, control=" + control + ")"

		else:
			self.openCorrelacion()

		self.ui.text_result.append("> " + comando)

		def f(x):
			self.ui.text_result.textCursor().insertText(x)

		rinterface.set_writeconsole(f)
		resultado = robjects.r(comando)
		self.ui.text_result.append(str(resultado))
		
		rinterface.set_writeconsole(rinterface.consolePrint)



	def cancel(self):
		print ("*Cancel*")



	def onClickPearsson(self):
		print "click Pearsson"
		self.dialogUi.groupBox_2.setHidden(False)
		self.dialogUi.groupBox_3.setHidden(False)
		self.dialogUi.groupBox_4.setHidden(False)
		self.dialogUi.groupBox_5.setHidden(True)
		self.dialogUi.groupBox_6.setHidden(False)
		self.dialogUi.setFixedHeight(450)



	def onClickCorica(self):
		print "click corica"
		self.dialogUi.groupBox_2.setHidden(False)
		self.dialogUi.groupBox_3.setHidden(True)
		self.dialogUi.groupBox_4.setHidden(True)
		self.dialogUi.groupBox_5.setHidden(True)
		self.dialogUi.groupBox_6.setHidden(True)
		self.dialogUi.setFixedHeight(200)



	def onClickParcial(self):
		print "click parcial-semi"
		self.dialogUi.groupBox_2.setHidden(False)
		self.dialogUi.groupBox_3.setHidden(True)
		self.dialogUi.groupBox_4.setHidden(True)
		self.dialogUi.groupBox_5.setHidden(False)
		self.dialogUi.groupBox_6.setHidden(True)
		self.dialogUi.setFixedHeight(300)




	def openMultiSelector_1(self):
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

		QtCore.QObject.connect(self.dialogMS.moveToLeft, QtCore.SIGNAL("clicked()"), self.moveToLeft_1, QtCore.Qt.UniqueConnection)
		QtCore.QObject.connect(self.dialogMS.moveToRight, QtCore.SIGNAL("clicked()"), self.moveToRight_1, QtCore.Qt.UniqueConnection)

		QtCore.QObject.connect(self.dialogMS.buttonBox, QtCore.SIGNAL("accepted()"), self.acceptMultiSelector_1, QtCore.Qt.UniqueConnection)
		QtCore.QObject.connect(self.dialogMS.buttonBox, QtCore.SIGNAL("rejected()"), self.cancel, QtCore.Qt.UniqueConnection)



	def openMultiSelector_2(self):
		self.dialogMS_2 = self.d_multiselector_2
		self.dialogMS_2.setWindowTitle("Selecciona los parametros")
		self.dialogMS_2.show()
		n_items = "length(names(datos))"
		n_items = robjects.r(n_items)
		n_items = n_items[0]

		self.dialogMS_2.selector_left.clear()
		self.dialogMS_2.selector_right.clear()
		for i in range(n_items):
			item_factor = "names(datos)[" + str(i+1) + "]"
			item_factor = robjects.r(item_factor)
			self.dialogMS_2.selector_left.insertItem(i, str(item_factor[0]))

		QtCore.QObject.connect(self.dialogMS_2.moveToLeft, QtCore.SIGNAL("clicked()"), self.moveToLeft_2, QtCore.Qt.UniqueConnection)
		QtCore.QObject.connect(self.dialogMS_2.moveToRight, QtCore.SIGNAL("clicked()"), self.moveToRight_2, QtCore.Qt.UniqueConnection)

		QtCore.QObject.connect(self.dialogMS_2.buttonBox, QtCore.SIGNAL("accepted()"), self.acceptMultiSelector_2, QtCore.Qt.UniqueConnection)
		QtCore.QObject.connect(self.dialogMS_2.buttonBox, QtCore.SIGNAL("rejected()"), self.cancel, QtCore.Qt.UniqueConnection)




	def moveToRight_1(self):
		self.dialogMS.selector_right.addItem(str(self.dialogMS.selector_left.currentItem().text()))
		self.dialogMS.selector_left.takeItem(self.dialogMS.selector_left.currentRow())
		print "right n=" + str(self.dialogMS.selector_right.count())
		print "left n=" + str(self.dialogMS.selector_left.count())



	def moveToLeft_1(self):
		self.dialogMS.selector_left.addItem(str(self.dialogMS.selector_right.currentItem().text()))
		self.dialogMS.selector_right.takeItem(self.dialogMS.selector_right.currentRow())
		print "right n=" + str(self.dialogMS.selector_right.count())
		print "left n=" + str(self.dialogMS.selector_left.count())


	def moveToRight_2(self):
		self.dialogMS_2.selector_right.addItem(str(self.dialogMS_2.selector_left.currentItem().text()))
		self.dialogMS_2.selector_left.takeItem(self.dialogMS_2.selector_left.currentRow())
		print "right n=" + str(self.dialogMS_2.selector_right.count())
		print "left n=" + str(self.dialogMS_2.selector_left.count())



	def moveToLeft_2(self):
		self.dialogMS_2.selector_left.addItem(str(self.dialogMS_2.selector_right.currentItem().text()))
		self.dialogMS_2.selector_right.takeItem(self.dialogMS_2.selector_right.currentRow())
		print "right n=" + str(self.dialogMS_2.selector_right.count())
		print "left n=" + str(self.dialogMS_2.selector_left.count())



	def acceptMultiSelector_1(self):
		self.listMulti_1 = []
		if (self.dialogMS.selector_right.count() > 0):
			for i in range(self.dialogMS.selector_right.count()):
				self.listMulti_1.append(self.dialogMS.selector_right.item(i).text())

		self.dialogUi.var_select_1.clear()
		self.dialogUi.var_select_1.insert(', '.join(self.listMulti_1))
		print "accept openMultiSelector_1"
		print ', '.join(self.listMulti_1)



	def acceptMultiSelector_2(self):
		self.listMulti_2 = []
		if (self.dialogMS_2.selector_right.count() > 0):
			for i in range(self.dialogMS_2.selector_right.count()):
				self.listMulti_2.append(self.dialogMS_2.selector_right.item(i).text())

		self.dialogUi.var_select_2.clear()
		self.dialogUi.var_select_2.insert(', '.join(self.listMulti_2))
		print "accept openMultiSelector_2"
		print ', '.join(self.listMulti_2)
