case class Fila(NumFila:Int,CReina:Int)
type Tablero = List[Fila] // List(
//---------------------AUX --------
def modulo(n:Int):Int ={
  if(n>0){
    n
  }else{
    -n
  }
}
//-------------- Verificaciones -----------
def hayReinasEnColumna(fila: Fila,columna:Int):Boolean = fila.CReina==columna

def cantReinaEnColumna(tablero:Tablero,columna:Int):Int= {
  tablero.indices.foldLeft(0)((acc,index) => if(hayReinasEnColumna(tablero(index),columna)) acc + 1 else acc)
}

def seComenHoV(tablero:Tablero):Boolean = {
  tablero.indices.foldLeft(false)((acc,index)=> if (cantReinaEnColumna(tablero,index) > 1) true else acc)
}

def hayReinaDiagonal(filaOrg:Fila,filaPrima:Fila): Boolean = modulo(filaOrg.NumFila - filaPrima.NumFila) == modulo(filaOrg.CReina - filaPrima.CReina )

def enDiagonal(tablero:Tablero,filaOrg:Int):Boolean = {
  tablero.indices.foldLeft(false)((acc, index)=> if ( index != filaOrg ) acc || hayReinaDiagonal(tablero(filaOrg),tablero(index))   else  acc)
}

def seComenDiagonal(tablero: Tablero): Boolean = {
  tablero.indices.foldLeft(false)((acc,index)=> if(enDiagonal(tablero,index)) true else acc)
}

//--------------------------------------------------------------------------------------
def esTableroValido(tablero: Tablero)= !seComenHoV(tablero) && !seComenDiagonal(tablero)
//----------------------------------------------------------------------------------------------------------------------

val tr41 =List(Fila(0,2),Fila(1,0),Fila(2,3),Fila(3,1))
seComenHoV(tr41)
seComenDiagonal(tr41)
esTableroValido(tr41)
val tr42 =List(Fila(0,1),Fila(1,3),Fila(2,0),Fila(3,2))
seComenHoV(tr42)
seComenDiagonal(tr42)
esTableroValido(tr42)
val tr43 =List(Fila(0,0),Fila(1,2),Fila(2,1),Fila(3,3))
seComenHoV(tr43)
seComenDiagonal(tr43)
esTableroValido(tr43)
val tr44 =List(Fila(0,0),Fila(1,1),Fila(2,1),Fila(3,3))
seComenHoV(tr44)
seComenDiagonal(tr44)
esTableroValido(tr44)




