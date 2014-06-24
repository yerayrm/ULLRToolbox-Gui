from PySide import QtCore, QtGui
from rpy2 import rinterface, robjects
import sys

class FncGrafPercenHisto():

	#Constructor
	def __init__(self, ui, d_percentiles, d_histograma, d_multiselector):
		self.ui = ui
		self.d_percentiles = d_percentiles
		self.d_histograma = d_histograma
		self.d_multiselector = d_multiselector

		QtCore.QObject.connect(self.ui.act_percentiles, QtCore.SIGNAL("triggered()"), self.openPercentiles)
		QtCore.QObject.connect(self.ui.act_histogramas, QtCore.SIGNAL("triggered()"), self.openHistograma)
		

	######################################################################################################
	### PERCENTILES
	def openPercentiles(self):
		self.dialogUi = self.d_percentiles
		self.dialogUi.setWindowTitle("Percentiles")
		self.dialogUi.show()

		# Inicializo los combobox
		def f(x):
			print x
		rinterface.set_writeconsole(f)
		n_items = "length(names(datos))"
		n_items = robjects.r(n_items)
		n_items = n_items[0]
		self.dialogUi.cb_variable.clear()
		self.dialogUi.cb_factor.clear()
		self.dialogUi.cb_factor.addItem(str("Sin factor"))
		for i in range(n_items):
			item_factor = "names(datos)[" + str(i+1) + "]"
			item_factor = robjects.r(item_factor)
			self.dialogUi.cb_variable.addItem(str(item_factor[0]))
			self.dialogUi.cb_factor.addItem(str(item_factor[0]))

		rinterface.set_writeconsole(rinterface.consolePrint)

		# Signals
		QtCore.QObject.connect(self.dialogUi.buttonBox, QtCore.SIGNAL("accepted()"), self.acceptPercentiles, QtCore.Qt.UniqueConnection)
		QtCore.QObject.connect(self.dialogUi.buttonBox, QtCore.SIGNAL("rejected()"), self.cancel, QtCore.Qt.UniqueConnection)


	def acceptPercentiles(self):
		print ("*Accept*")
		variable = str(self.dialogUi.cb_variable.currentText())
		factor = str(self.dialogUi.cb_factor.currentText())
		percentiles = self.dialogUi.le_percentiles.text()

		if (len(percentiles) > 0):
			percentiles = ", percentiles=c(" + percentiles + ")"
		else:
			percentiles = ""

		if (factor == "Sin factor"):
			comando = "percentiles.fnc(datos, variable='" + variable + "'" + percentiles + ")"
		else:
			comando = "percentiles.fnc(datos, variable='" + variable + "', que.factor='" + factor + "'" + percentiles + ")"


		self.ui.text_result.append("> " + comando)

		def f(x):
			self.ui.text_result.textCursor().insertText(x)

		rinterface.set_writeconsole(f)
		resultado = robjects.r(comando)
		self.ui.text_result.append(str(resultado))
		
		rinterface.set_writeconsole(rinterface.consolePrint)



	def cancel(self):
		print ("*Cancel*")



	######################################################################################################
	### HISTOGRAMAS
	def openHistograma(self):
		self.dialogUi = self.d_histograma
		self.dialogUi.setWindowTitle("Histograma")
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
		self.dialogUi.cb_variable.clear()
		for i in range(n_items):
			item_factor = "names(datos)[" + str(i+1) + "]"
			item_factor = robjects.r(item_factor)
			self.dialogUi.cb_variable.addItem(str(item_factor[0]))

		rinterface.set_writeconsole(rinterface.consolePrint)

		# Signals
		QtCore.QObject.connect(self.dialogUi.buttonBox, QtCore.SIGNAL("accepted()"), self.acceptHistograma, QtCore.Qt.UniqueConnection)
		QtCore.QObject.connect(self.dialogUi.buttonBox, QtCore.SIGNAL("rejected()"), self.cancel, QtCore.Qt.UniqueConnection)



	def acceptHistograma(self):
		print ("*Accept*")
		#variable
		variable = str(self.dialogUi.cb_variable.currentText())
		#factores
		if (len(self.listMulti) > 0):
			factores = ":".join(self.listMulti)
			factores = ", que.factor='" + factores + "'"
		else:
			factores = ""
		#orden -> orden=c(1,2)
		col = self.dialogUi.le_col.text()
		fil = self.dialogUi.le_fil.text()
		if (len(col) > 0):
			orden = ", orden=c(" + col + ", " + fil + ")"
		else:
			orden = ""

		#cortes -> cortes=c(-2,2)
		izda = self.dialogUi.le_izda.text()
		dcha = self.dialogUi.le_dcha.text()
		if (len(izda) > 0):
			cortes = ", cortes=c(" + izda + ", " + dcha + ")"
		else:
			cortes = ""

		#parametro tipificado
		if self.dialogUi.check_tipificados.isChecked():
			tipica = ", p.tipica=T"
		else:
			tipica = ""
		#parametro misma grafica
		if self.dialogUi.check_onlyone.isChecked():
			check = ", check=T"
		else:
			check = ""

		comando = "histograma.fnc(datos, vd='" + variable + "'" + factores + tipica + check + orden + cortes + ")"
		
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


