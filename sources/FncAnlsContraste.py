#!/usr/bin/env python
# -*- coding: utf-8 -*-

from PySide import QtCore, QtGui
from rpy2 import rinterface, robjects
import sys

class FncAnlsContraste():

	#Constructor
	def __init__(self, ui, d_contr_inter, d_contr_intra, d_multiselector):
		self.ui = ui
		self.d_contr_inter = d_contr_inter
		self.d_contr_intra = d_contr_intra
		self.d_multiselector = d_multiselector

		QtCore.QObject.connect(self.ui.act_contr_inter, QtCore.SIGNAL("triggered()"), self.openContrInter)
		QtCore.QObject.connect(self.ui.act_contr_intra, QtCore.SIGNAL("triggered()"), self.openContrIntra)


	######################################################################################################
	### CONSTRASTE DE MEDIAS -> INTERGRUPO
	def openContrInter(self):
		self.dialogUi = self.d_contr_inter
		self.dialogUi.setWindowTitle(("Contraste T intergrupo").decode("utf8"))
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
		self.dialogUi.cb_vi.clear()
		for i in range(n_items):
			item_factor = "names(datos)[" + str(i+1) + "]"
			item_factor = robjects.r(item_factor)
			self.dialogUi.cb_vi.addItem(str(item_factor[0]))

		rinterface.set_writeconsole(rinterface.consolePrint)

		# Signals
		QtCore.QObject.connect(self.dialogUi.buttonBox, QtCore.SIGNAL("accepted()"), self.acceptIntergrupo, QtCore.Qt.UniqueConnection)
		QtCore.QObject.connect(self.dialogUi.buttonBox, QtCore.SIGNAL("rejected()"), self.cancel, QtCore.Qt.UniqueConnection)



	def acceptIntergrupo(self):
		print ("*Accept*")

		vd = "c('"
		vd = vd + "', '".join(self.listMulti)
		vd = vd + "')"
		vi = str(self.dialogUi.cb_vi.currentText())

		if self.dialogUi.check_cajas.isChecked():
			comando = "contraste.t.intergrupo.fnc(datos, vd=" + vd + ", vi='" + vi + "', identifica=T)"
		else:
			comando = "contraste.t.intergrupo.fnc(datos, vd=" + vd + ", vi='" + vi + "')"

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
	### CONTRASTE DE MEDIAS -> INTRAGRUPO
	def openContrIntra(self):
		self.dialogUi = self.d_contr_intra
		self.dialogUi.setWindowTitle(("Contraste T intragrupo").decode("utf8"))
		self.dialogUi.show()

		# Inicializo los combobox
		def f(x):
			print x
		rinterface.set_writeconsole(f)
		n_items = "length(names(datos))"
		n_items = robjects.r(n_items)
		n_items = n_items[0]
		self.dialogUi.cb_var1.clear()
		self.dialogUi.cb_var2.clear()
		for i in range(n_items):
			item_factor = "names(datos)[" + str(i+1) + "]"
			item_factor = robjects.r(item_factor)
			self.dialogUi.cb_var1.addItem(str(item_factor[0]))
			self.dialogUi.cb_var2.addItem(str(item_factor[0]))

		rinterface.set_writeconsole(rinterface.consolePrint)

		# Signals
		QtCore.QObject.connect(self.dialogUi.buttonBox, QtCore.SIGNAL("accepted()"), self.acceptIntragrupo, QtCore.Qt.UniqueConnection)
		QtCore.QObject.connect(self.dialogUi.buttonBox, QtCore.SIGNAL("rejected()"), self.cancel, QtCore.Qt.UniqueConnection)



	def acceptIntragrupo(self):
		print ("*Accept*")

		variable_1 = str(self.dialogUi.cb_var1.currentText())
		variable_2 = str(self.dialogUi.cb_var2.currentText())

		if self.dialogUi.check_opc.isChecked():
			comando = "contraste.t.intragrupo.fnc(OBrienKaiser, par=c('" + variable_1 + "','" + variable_2 + "'), elimina.outlier=T)"
		else:
			comando = "contraste.t.intragrupo.fnc(OBrienKaiser, par=c('" + variable_1 + "','" + variable_2 + "'))"

		self.ui.text_result.append("> " + comando)

		def f(x):
			self.ui.text_result.textCursor().insertText(x)

		rinterface.set_writeconsole(f)
		resultado = robjects.r(comando)
		self.ui.text_result.append(str(resultado))
		
		rinterface.set_writeconsole(rinterface.consolePrint)

