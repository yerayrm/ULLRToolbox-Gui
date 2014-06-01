#!/usr/bin/env python
# -*- coding: utf-8 -*-

from PySide import QtCore, QtGui
from rpy2 import rinterface, robjects
import sys

class FncAnlsAnova():

	#Constructor
	def __init__(self, ui, d_anova_inter, d_anova_intra, d_anova_split, d_multiselector):
		self.ui = ui
		self.d_anova_inter = d_anova_inter
		self.d_anova_intra = d_anova_intra
		self.d_anova_split = d_anova_split
		self.d_multiselector = d_multiselector

		QtCore.QObject.connect(self.ui.act_anv_intergrupo, QtCore.SIGNAL("triggered()"), self.openAnovaInter)
		QtCore.QObject.connect(self.ui.act_anv_intragrupo, QtCore.SIGNAL("triggered()"), self.openAnovaIntra)
		QtCore.QObject.connect(self.ui.act_anv_splitplot, QtCore.SIGNAL("triggered()"), self.openAnovaSplit)
		

	######################################################################################################
	### ANOVA -> INTERGRUPO
	def openAnovaInter(self):
		self.dialogUi = self.d_anova_inter
		self.dialogUi.setWindowTitle("ANOVA intergrupo".decode("utf8"))
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
		QtCore.QObject.connect(self.dialogUi.buttonBox, QtCore.SIGNAL("accepted()"), self.acceptIntergrupo, QtCore.Qt.UniqueConnection)
		QtCore.QObject.connect(self.dialogUi.buttonBox, QtCore.SIGNAL("rejected()"), self.cancel, QtCore.Qt.UniqueConnection)



	def acceptIntergrupo(self):
		print ("*Accept*")

		fac_inter = "c('"
		fac_inter = fac_inter + "', '".join(self.listMulti)
		fac_inter = fac_inter + "')"
		vd = str(self.dialogUi.cb_variable.currentText())

		if self.dialogUi.rb_tipo2.isChecked():
			tipo = ", tipo=2"
		elif self.dialogUi.rb_tipo3.isChecked():
			tipo = ", tipo=3"
		else:
			tipo = ""

		if self.dialogUi.rb_tipo3.isChecked():
			pdf = ", to.pdf=T"
		else:
			pdf = ""

		comando = "Anova.fnc(datos, fac.inter=" + fac_inter + ", vd='" + vd + "'" + tipo + pdf + ")"


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
	### ANOVA -> INTRAGRUPO
	def openAnovaIntra(self):
		self.dialogUi = self.d_anova_intra
		self.dialogUi.setWindowTitle("ANOVA intragrupo".decode("utf8"))
		self.dialogUi.show()

		QtCore.QObject.connect(self.dialogUi.addRow, QtCore.SIGNAL("clicked()"), self.addNewFactor, QtCore.Qt.UniqueConnection)
		
		# Signals
		QtCore.QObject.connect(self.dialogUi.buttonBox, QtCore.SIGNAL("accepted()"), self.acceptIntragrupo, QtCore.Qt.UniqueConnection)
		QtCore.QObject.connect(self.dialogUi.buttonBox, QtCore.SIGNAL("rejected()"), self.cancel, QtCore.Qt.UniqueConnection)



	def addNewFactor(self):
		row = self.dialogUi.table_factor.rowCount()
		self.dialogUi.table_factor.insertRow(row)



	def acceptIntragrupo(self):
		print ("*Accept*")

		fac_intra = "fac.intra=list("
		for i in range(self.dialogUi.table_factor.rowCount()):
			itemName = self.dialogUi.table_factor.item(i, 0).text()
			itemParams = self.dialogUi.table_factor.item(i, 1).text()
			fac_intra = fac_intra + itemName + "=" + itemParams
			if (i != self.dialogUi.table_factor.rowCount()-1):
				fac_intra = fac_intra + ", "

		fac_intra = fac_intra + ")"
		print fac_intra

		col_begin = self.dialogUi.col_begin.text()

		if self.dialogUi.check_graficas.isChecked():
			graficas = ", grafica=T"
		else:
			graficas = ", grafica=F"

		graficas = "" #hay un error en el toolbox que no permite ejecutar el comando

		comando = "Anova.fnc(datos, " + fac_intra + ", col.empieza.mr=" + col_begin + graficas + ")"

		self.ui.text_result.append("> " + comando)

		def f(x):
			self.ui.text_result.textCursor().insertText(x.decode("utf8"))

		rinterface.set_writeconsole(f)
		resultado = robjects.r(comando)
		self.ui.text_result.append(str(resultado.decode("utf8")))
		
		rinterface.set_writeconsole(rinterface.consolePrint)




	######################################################################################################
	### ANOVA -> SPLIT-PLOT
	def openAnovaSplit(self):
		self.dialogUi = self.d_anova_split
		self.dialogUi.setWindowTitle("ANOVA split-plot".decode("utf8"))
		self.dialogUi.show()

		# Abro el multiselector
		self.dialogUi.var_select.clear()
		QtCore.QObject.connect(self.dialogUi.pushMultiSelector, QtCore.SIGNAL("clicked()"), self.openMultiSelector, QtCore.Qt.UniqueConnection)

		# Añadir una fila a la tabla
		QtCore.QObject.connect(self.dialogUi.addRow, QtCore.SIGNAL("clicked()"), self.addNewFactor, QtCore.Qt.UniqueConnection)

		# Signals
		QtCore.QObject.connect(self.dialogUi.buttonBox, QtCore.SIGNAL("accepted()"), self.acceptSplit, QtCore.Qt.UniqueConnection)
		QtCore.QObject.connect(self.dialogUi.buttonBox, QtCore.SIGNAL("rejected()"), self.cancel, QtCore.Qt.UniqueConnection)



	def acceptSplit(self):
		print ("*Accept*")

		#fac.inter
		fac_inter = "c('"
		fac_inter = fac_inter + "', '".join(self.listMulti)
		fac_inter = fac_inter + "')"

		#fac.intra
		fac_intra = "fac.intra=list("
		for i in range(self.dialogUi.table_factor.rowCount()):
			itemName = self.dialogUi.table_factor.item(i, 0).text()
			itemParams = self.dialogUi.table_factor.item(i, 1).text()
			fac_intra = fac_intra + itemName + "=" + itemParams
			if (i != self.dialogUi.table_factor.rowCount()-1):
				fac_intra = fac_intra + ", "

		fac_intra = fac_intra + ")"
		print fac_intra

		#col.empieza
		col_begin = self.dialogUi.col_begin.text()

		#tipo de nivel
		if self.dialogUi.btn_tipo2.isChecked():
			tipo = ", tipo=2"
		elif self.dialogUi.btn_tipo3.isChecked():
			tipo = ", tipo=3"
		else:
			tipo = ""

		#ylim
		limit_sup = self.dialogUi.limit_sup.text()
		limit_inf = self.dialogUi.limit_inf.text()
		ylim = ", ylim=c(" + limit_inf + "," + limit_sup + ")"

		#comando
		comando = "Anova.fnc(datos, fac.inter=" + fac_inter + ", " + fac_intra + ", col.empieza.mr=" + col_begin + ylim + tipo + ")"

		self.ui.text_result.append("> " + comando)

		def f(x):
			self.ui.text_result.textCursor().insertText(x.decode("utf8"))

		rinterface.set_writeconsole(f)
		resultado = robjects.r(comando)
		self.ui.text_result.append(str(resultado.decode("utf8")))
		
		rinterface.set_writeconsole(rinterface.consolePrint)
