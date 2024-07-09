type Ficha = Int
type Torre = List[Ficha]
type Tablero = List[Torre]
type Movimiento=(Ficha,Int,Int)
type ListMov =List[Movimiento]

case class Juego (tablero:Tablero,listMov: ListMov)
/////////////////////////////////////////////////////////////////////////////
def torreInicial(cant:Int) : List[Int]=
  cant match {
    case  0 => Nil
    case _ =>  torreInicial(cant-1).appended(cant)
  }
def armarTorre( torre:Torre, yo:Int,orig:Int,dest:Int,ficha:Ficha):Torre = {
  yo match {
    case yo if yo == orig => torre.tail
    case yo if yo == dest => ficha :: torre
    case _ => torre
  }
}


def tablero (tablero:Tablero, orig:Int,dest:Int) :Tablero = List(
  armarTorre(tablero(0), 0, orig, dest, tablero(orig).head),
  armarTorre(tablero(1), 1, orig, dest, tablero(orig).head),
  armarTorre(tablero(2), 2, orig, dest, tablero(orig).head),
)
def addMov(ficha:Ficha,orig:Int,dest:Int):Movimiento= (ficha,orig, dest)
/*
 fx mover hace un movimiento devuelve el juego con la pieza movida va a llamar a tablero()
*/
def jugada(juego:Juego,orig:Int,dest:Int) :Juego = Juego(
  tablero(juego.tablero,orig,dest),
  juego.listMov.appended(addMov(juego.tablero(orig).head ,orig,dest ) ))

/////////////////////////////////////////////////////////////////////////////////////////////////////

def resolverHanoi (juego:Juego, cantFicha:Ficha, orig:Int, dest:Int, aux:Int) :Juego =

  if (cantFicha == 0){
    juego
  }else {
    val j1 = resolverHanoi(juego, cantFicha - 1, orig, aux, dest)
    val j2 = jugada(j1,orig, dest)
    resolverHanoi(j2, cantFicha - 1, aux, dest, orig)
  }


/*
def resolverHanoi(juego:Juego,cantFicha:Ficha,orig:Int, aux:Int, dest:Int):Juego =
  cantFicha match{
    case cantFicha if cantFicha == 0 => juego
    case cantFicha if cantFicha > 0 => resolverHanoi(jugada(juego,orig,dest),cantFicha-1,orig,dest,aux)
  }
*/
/////////////////////////////////////////////////////////////////////////////////////////////////////


def hanoi(cant:Int):ListMov = resolverHanoi(Juego(List( torreInicial(cant),List(),List() ), List()), cant, 0, 2, 1).listMov


//hanoi(0)
hanoi(1)
hanoi(2)
hanoi(3)
hanoi(4)






