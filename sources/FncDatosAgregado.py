from PySide import QtCore, QtGui
from rpy2 import rinterface, robjects
import sys

class FncDatosAgregado():

	#Constructor
	def __init__(self, ui, d_agregado):
		self.ui = ui
		self.d_agregado = d_agregado

		QtCore.QObject.connect(self.ui.act_agregado, QtCore.SIGNAL("triggered()"), self.openAgregadoDialog)



	def openAgregadoDialog(self):
		self.dialogUi = self.d_agregado
		self.dialogUi.setWindowTitle("Agregado")
		self.dialogUi.show()
		self.dialogUi.te_matriz.clear()
		self.dialogUi.te_matriz.append("list(A=c('a1','a2'), B=c('b1','b2'))")
		self.dialogUi.buttonBox.accepted.connect(self.acceptAgregado)
		self.dialogUi.buttonBox.rejected.connect(self.cancel)



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

