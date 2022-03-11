import sys
import copy

class Tree:
    #Classe abstracta
    
    class Position:
        #classe representa la localitzacio d'un objecte
        def element(self):
            raise NotImplementedError("Ha d'estar implementat per la subclasse")
    
        def __eq__(self,other):
            raise NotImplementedError("Ha d'estar implementat per la subclasse")
    
        def __ne__(self,other):
            raise NotImplementedError("Ha d'estar implementat per la subclasse")
    
    #Metodes ABSTRACTES QUE LES CLASSES CONCRETES HAN DE TENIR
    def arrel(self):
        #Retorna la posicio que representa arrel
        raise NotImplementedError("Ha d'estar implementat per la subclasse")
        
    def pare(self,p):
        #Retorna la posicio que representa pare
        raise NotImplementedError("Ha d'estar implementat per la subclasse")
    
    def numFills(self,p):
        #Retorna el nombre de fills de p
        raise NotImplementedError("Ha d'estar implementat per la subclasse")
    
    def fills(self,p):
        #Retorna una iteracio de les posicions que representen els fills de p
        raise NotImplementedError("Ha d'estar implementat per la subclasse")
    
    def __len__(self):
        #Retorna el nombre total delements a larbre
        raise NotImplementedError("Ha d'estar implementat per la subclasse")
        
    #Metodes Concrets de la classe abstracta
    
    def esArrel(self,p):
        return self.arrel()==p
    
    def esFulla(self,p):
        return self.numFills(p)==0
    
    def esBuit(self):
        return len(self)==0
    
    def profunditat(self,p):
        if self.esArrel(p):
            return 0
        else:
            return 1+self.profunditat(self.pare(p))
        
    def _alcada(self,p):
        if self.esFulla(p):
            return 0
        else:
            return 1+max(self._alcada(c) for c in self.fills(p))
    
    def alcada(self,p=None):
        if p is None:
            p=self.arrel()
        return self._alcada(p)


    def preorder(self):
        if not self.esBuit():
            for p in self._subTreePreorder(self.arrel()):
                yield p
                
    def _subTreePreorder(self,p):
        yield p
        for fillN in self.fills(p):
            for n in self._subTreePreorder(fillN):
                yield n                
        
    def __iter__(self):
        for p in self.positions():
            yield p.element()
            
    def positions(self):
        return self.preorder()    
    
    def __str__(self):        
        if self._size>0:
            return self.preorderIndent(self.arrel(),0)
        else:
            return ""
        
    
    def preorderIndent(self, p,depth):        
        cadRes = 2 * depth * " " + str(p.element())+"\n"
        for n in self.fills(p):
            cadRes += self.preorderIndent(n,depth+1)
        return cadRes
    
    
class BinaryTree(Tree):
    def left(self, p):
        """Retorna posicio del fill esquerre de p o None si no existeix"""
        raise NotImplementedError( "Ha d'estar implementat per la subclasse")

    def right(self, p):
        """Retorna posicio representa fill dret de p o None si no existeix"""
        raise NotImplementedError( "Ha d'estar implementat per la subclasse")

    # ---------- Metodes concrets implementats a aquesta classe
    def germans(self, p):
        """Return a Position representing p s sibling (or None if no sibling)."""
        pare = self.pare(p)
        if pare is None:
            return None
        else:
            if p == self.left(pare):
                return self.right(pare)
            else:
                return self.left(pare)

    def fills(self, p):
        """Genera iteracio de les posicions representant els fills de p"""
        if self.left(p) is not None:
            yield self.left(p)
        if self.right(p) is not None:
            yield self.right(p)
            
    def checkInt(self,s):
        if s[0] in ('-', '+'):
            return s[1:].isdigit()
        return s.isdigit()
    
    def read(self,nomF):
        with open(nomF,"rt") as file:
            line = file.readline()
            if line: 
                h=int(line)
                line = file.readline()
                if line: 
                    v = line.split()
                    if len(v)>1:
                        #print("llegint valor",v)
                        posArrel=self._addArrel(h,[int(par) if self.checkInt(par) else par for par in v[1:]])
                        self.llegirTreeRec(h,file,posArrel,"ESQ")                                            
                        self.llegirTreeRec(h,file,posArrel,"DRET")                                            
                            
    def llegirTreeRec(self,h,file,pare,tipFillED):
        if (h > 0):
            line = file.readline()
            if line: 
                val = line.split()
                if len(val)>1:
                    posPareAct=None
                    #Creem arrel del subarbre actual
                    if (tipFillED=="ESQ"):
                        #subarbre actual sera fill esquerre del seu pare                        
                        posPareAct=self._addLeft(pare, [int(par) if self.checkInt(par) else par for par in val[1:]])                        
                    else:
                        #subarbre actual sera fill dret del seu pare
                        posPareAct=self._addRight(pare, [int(par) if self.checkInt(par) else par for par in val[1:]])                        
                    #Creem fill esquerre
                    h-=1
                    self.llegirTreeRec(h,file,posPareAct,"ESQ")
                    #Creem fill dret
                    self.llegirTreeRec(h,file,posPareAct,"DRET")                
    
    def inorder(self):
        if not self.esBuit():
            for p in self._subTreeInorder(self.arrel()):
                yield p
                
    def _subTreeInorder(self,p):
        if self.left(p) is not None:
            for n in self._subTreeInorder(self.left(p)):
                yield n
        yield p
        if self.right(p) is not None: 
            for n in self._subTreeInorder(self.right(p)):
                yield n
    
    def positions(self):
        return self.inorder()    
    
    def cerca(self, val):
        if self.esBuit():
            return None
        else:
            return self.cercaRec(val,self.arrel())
    
    def cercaRec(self, val,pos):
        if pos.element()==val:           
            return pos
        elif val < pos.element():
            left=self.left(pos)
            if left!=None:
                return self.cercaRec(val, left)
            else:
                return None
        else:            
            right=self.right(pos)
            if right!=None:
                return self.cercaRec(val, right)
            else:
                return None      
            
class BinaryTreeLink(BinaryTree):
    """Representacio lincada arbre binari"""

    class _Node:
        __slots__ = '_element' , '_pare' , '_left' , '_right'
        
        def __init__ (self, element, pare=None, left=None, right=None):
            self._element = element
            self._pare = pare
            self._left = left
            self._right = right

    class Position(BinaryTree.Position):
        """Abstraccio representa localitzacio un sol element"""

        def __init__(self, container, node):
            self._container = container
            self._node = node
        def element(self):
            return self._node._element
        
        def node(self):
            return self._node
        def __eq__(self, other):
            """Retorna True si other es una posicio representant l amateixa localitzacion"""
            return type(other) is type(self) and other._node is self._node
        
        def __ne__(self,other):
            return not self==other
        
        def __str__(self):
            return str(self.element())

    def _validate(self, p):
        """Retorna una posicio associada si es valid"""
        if not isinstance(p, self.Position):
            raise sys.TypeError( "p ha de ser de tipus Position")
        if p._container is not self:
            raise sys.ValueError("p no pertany a aquest container")
        if p._node._pare is p._node: # convention for deprecated nodes
            raise sys.ValueError("p ja no es valid")
        return p._node

    def _makePosition(self, node):
        """Retorna una instancia de Position per un node donat o None si nbo hi ha node"""      
        return self.Position(self, node) if node is not None else None
        
    
    #-------------------------- constructor del BinaryTree --------------------------
    def __init__ (self,value=None):
        self._arrel = None
        self._size = 0
        if value!=None:self._addArrel(0,value)
            
        
                   
    #-------------------------- public accessors --------------------------
    def __len__(self):
        """Retorna nombre total d elements al Tree"""
        return self._size

    def arrel(self):
        """Retorna Position de l arrel o None si Tree buit """
        return self._makePosition(self._arrel)

    def pare(self, p):
        """Return Position del pare de p o None si p es arrel"""
        node = self._validate(p)
        return self._makePosition(node._pare)

    def left(self, p):
        """Return Position de left de p o None si no te left"""
        node = self._validate(p)
        return self._makePosition(node._left)

    def right(self, p):
        """Return Position de right de p o None si no te right"""
        node = self._validate(p)
        return self._makePosition(node._right)

    def numFills(self, p):
        """Return el nombre de fills de p"""        
        node = self._validate(p)
        count = 0
        if node._left is not None: # left child exists
            count += 1
        if node._right is not None: # right child exists
            count += 1
        return count    
    
    def _addArrel(self, h,e):
        """Posa e com a arrel a un Tree buit. Raise error si Tree no esta buit"""
        
        if self._arrel is not None: raise sys.ValueError( "Arrel ja existeix")
        self._size = 1
        self._arrel= self._Node(e)
        return self._makePosition(self._arrel)

    def _addLeft(self, p, e):
        """Crea fill left a la Position p, i posa element e
        retorna Position del nou node
        retorna error si la position p no es valida o 
        si p ja te fill left 
        """        
        node = self._validate(p)
        if node._left is not None: raise sys.ValueError("Fill left existeix ja" )
        self._size += 1
        node._left = self._Node(e, node) # node is its parent
        return self._makePosition(node._left)

    def _addRight(self, p, e):
        """Crea fill right a la Position p, i posa element e
        retorna Position del nou node
        retorna error si la position p no es valida o 
        si p ja te fill right
        """        
        node = self._validate(p)
        if node._right is not None: raise sys.ValueError( "Fill right existeix ja")
        self._size += 1
        node._right = self._Node(e, node) # node is its parent
        return self._makePosition(node._right)

    def _replace(self, p, e):
        """Remplaca element de la position p amb e i retorna element antic"""
        node = self._validate(p)
        old = node._element
        node._element = e
        return old
    
    def _delete(self, p):
        """Borra node a position p i el reemplaca amb el seu fill si te.
           Return element guardat a Position p.
           o Raise ValueError si Position p es invalid o p te 2 fills
        """
        node = self._validate(p)
        if self.numFills(p) == 2: raise sys.ValueError(" p te 2 fills" )
        child = node._left if node._left else node._right 
        if child is not None:
            child._pare = node._pare # child s grandparent becomes parent
        if node is self._arrel:
            self._arrel = child # child becomes root
        else:
            pare = node._pare
            if node is pare._left:
                pare._left = child
            else:
                pare._right = child
        self._size -= 1
        node._pare = node # convention for deprecated node
        return node._element

    def _attach(self, p, t1, t2):
        """Afegeix Trees t1 i t2 com a left i right de p respectivamen"""
        node = self._validate(p)
        if not self.esFulla(p): raise sys.ValueError("Position ha de ser fulla")
        if not type(self) is type(t1) is type(t2):
            raise sys.TypeError( "Han de ser del mateix tipus Tree")
        self._size += len(t1) + len(t2)
        if not t1.esBuit( ): # attached t1 as left subtree of node
            t1._arrel._pare = node
            node._left = t1._arrel
            t1._arrel = None # set t1 instance to empty
            t1._size = 0
        if not t2.esBuit( ):
            t2._arrel._pare = node
            node._right = t2._arrel
            t2._arrel = None # set t2 instance to empty
            t2._size = 0
                
class BinaryTreeHeap(BinaryTree):
    #Heap de maxims
    class _Node:
        __slots__ = '_key','_value'       
        def __init__ (self, key,value):
            self._key= key
            self._value= value
        @property
        def key(self):
            return self._key
        @key.setter
        def key(self,key):
            self._key=key  
        @property
        def keyIndex(self):
            if type(self._key)==list:
                return self._key[0]
            else:
                return self._key
        @property
        def value(self):
            return self._value
        @value.setter
        def value(self,valor):
            self._value=valor  
            
        def __lt__(self, val):
            return self._value<val._value
        def __gt__(self, val):
            return self._value>val._value
        
        def __str__(self):
            return "NODE:("+str(self.key)+","+str(self.value)+")" 

    class Position(BinaryTree.Position):
        """Abstraccio representa localitzacio un sol element"""
        def __init__(self, container ,index,node):
            self._container =container
            self._index = index            
            self._node = node
            
        def key(self):
            return self._node._key
        
        def value(self):
            return self._node._value
        def element(self):
            return self._node._key,self._node._value
        
        def node(self):
            return self._node
        
        @property
        def index(self):
            return self._index
        @index.setter
        def pos(self,index):
            self._index=index
        
        
        def __eq__(self, other):
            """Retorna True si other es una posicio representant l amateixa localitzacion"""
            return type(other) is type(self) and other._node is self._node
        
        def __ne__(self,other):
            return not self==other
        
        def __str__(self):
            return "POSITION:("+str(self._node._key)+","+str(self._node._value)+")" 

    def _validate(self, p):
        """Retorna una posicio associada si es valid"""
        if not isinstance(p, self.Position):
            raise TypeError( "p ha de ser de tipus Position")
        if p._container is not self:
            raise ValueError("p no pertany a aquest container")
        if p.index <0 or p.index>= self._size or p.index >= len(self._data) :
            raise ValueError("p apunta a un node amb index incorrecte")
        return p._node

    def _makePosition(self, pos,node):
        """Retorna una instancia de Position per un node donat o None si nbo hi ha node"""      
        return self.Position(self, pos, node) if node is not None else None
    
    #-------------------------- constructor del BinaryTree --------------------------
    def __init__ (self,h=0):
        self._data = [self._Node(-1,-1)]*(2**(h+1)-1)
        self._size = 0
        self._index = [-1]*(2**(h+1)-1)
                   
    #-------------------------- public accessors --------------------------
    def __len__(self):
        """Retorna nombre total d elements al Tree"""
        return self._size

    def arrel(self):
        """Retorna Position de l arrel o None si Tree buit """
        if self._size>0:
            return self._makePosition(0,self._data[0])
        else:
            return None
        
    def pare(self, p):
        """Return Position del pare de p o None si p es arrel"""
        self._validate(p)
        indPare = (p.index-1) // 2
        if self._size>indPare and indPare >= 0:
            return self._makePosition(indPare,self._data[indPare])
        else:
            return None

    def indPare(self, indAct):
        """Return Position del pare de p o None si p es arrel"""
        indPar = (indAct-1) // 2
        if self._size>indPar:
            return indPar
        else:
            return None

    def left(self, p):
        """Return Position de left de p o None si no te left"""
        self._validate(p)
        indLeft = (2 * p.index) +1
        if self._size > indLeft:
            return self._makePosition(indLeft,self._data[indLeft])
        else: 
            return None
    
    def indLeft(self, indAct):
        """Return Position de left de p o None si no te left"""
        indLeft = (2 * indAct) +1
        if self._size > indLeft:
            return indLeft
        else: 
            return None
        
    def right(self, p):
        """Return Position de right de p o None si no te right"""
        self._validate(p)
        indRight= (2 * p.index) +2
        if self._size > indRight:
            return self._makePosition(indRight,self._data[indRight])
        else: 
            return None
    
    def indRight(self, indAct):
        """Return Position de right de p o None si no te right"""
        indRight= (2 * indAct) +2
        if self._size > indRight:
            return indRight
        else: 
            return None
        
    def numFills(self, p):
        """Return el nombre de fills de p"""        
        node = self._validate(p)
        count = 0
        if node.left() is not None: # left child exists
            count += 1
        if node.right() is not None: # right child exists
            count += 1
        return count   
    
    def modifElem(self,key,val):
        indexAct=self._index[key[0]]
        if self._validaIndex(indexAct):
            self._data[indexAct].value=val
            self._data[indexAct].key=key
            indfEsq= self.indLeft(indexAct)
            indfDret= self.indRight(indexAct)
            indPare= self.indPare(indexAct)
            if self._validaIndex(indfEsq):
                if self._data[indexAct].value < self._data[indfEsq].value:
                    self._downheap(indexAct)
                    return
            if self._validaIndex(indfDret):
                if self._data[indexAct].value < self._data[indfDret].value:
                    #descendir
                    self._downheap(indexAct)
                    return
            if self._validaIndex(indPare):
                if self._data[indexAct].value > self._data[indPare].value:
                    self._upheap(indexAct)
    
    def _swap(self, ind1, ind2):
        """Swap the elements at positions p1, p2."""
        if self._validaIndex(ind1) and self._validaIndex(ind2):
            self._data[ind1].key, self._data[ind2].key = self._data[ind2].key, self._data[ind1].key
            self._data[ind1].value, self._data[ind2].value= self._data[ind2].value, self._data[ind1].value
            self._index[self._data[ind1].keyIndex],self._index[self._data[ind2].keyIndex]  =  self._index[self._data[ind2].keyIndex],self._index[self._data[ind1].keyIndex]
        return ind2, ind1

    def _upheap(self, index):
        if (self._validaIndex(index)):
            indPare=self.indPare(index)
            if self._validaIndex(indPare) and self._data[indPare]< self._data[index]:                
                index, indPare = self._swap(index, indPare)
                self._upheap(index) # recur at position of parent
            
    def _validaIndex(self,ind):
        if ind!=None:
            return ind>=0 and ind<self._size
        else:
            return None
    
    def _downheap(self, index):
        if self._validaIndex(index):
            indBig=index
            indLeft = self.indLeft(index)
            if self._validaIndex(indLeft):
                if self._data[indLeft] > self._data[indBig]:
                    indBig=indLeft                
            indRight=self.indRight(index)
            if self._validaIndex(indRight):
                if self._data[indRight] > self._data[indBig]:
                    indBig = indRight                
            if indBig != index:
                indBig,index = self._swap(indBig, index)
                self._downheap(index)
    
    #------------------------------ public behaviors ------------------------------
    def clear(self):
        """Create a new empty Priority Queue."""
        self._data = []
        self._size=0
        self._index = []
        
    def resize(self,maxmida):
        """Create a new empty Priority Queue."""
        self._data = [self._Node(-1,-1)]*maxmida
        self._size=0
        self._index = [-1]*maxmida
        
    def _addArrel(self,h,val):        
        self._data = [self._Node(-1,-1)]*(2**(h+1)-1)
        self.add(h,val)     
        self._index = [-1]*(2**(h+1)-1)        

    def _addLeft(self,h,val):        
        self.add(h,val)      
        
    def _addRight(self,h, val):        
        self.add(h,val)      
        
    def add(self, h, element):
        """Add a key-value pair to the priority queue."""        
        if len(self._data)>self._size:
            self._data[self._size]=self._Node(element[0],element[1])
            self._index[self._data[self._size].keyIndex]=self._size
            self._size+=1
            self._upheap(self._size-1) # upheap newly added position
            
    def maxim(self):
        """Return but do not remove (k,v) tuple with minimum key.

        Raise Empty exception if empty.
        """
        if len(self)==0:
            raise ValueError(' Priority queue is empty. ')
        item = self._data[0]
        return (item._key,item._value)

    def remove_max(self):
        """Remove and return (k,v) tuple with minimum key.

        Raise Empty exception if empty.
        """        
        if self._size == 0:
            raise ValueError(' Priority queue is empty. ')      
        item = (copy.copy(self._data[0].key),self._data[0]._value) # and remove it from the list;      
        if self._size > 1:
            self._swap(0,self._size-1) # put minimum item at the end
            self._size-=1            
            self._downheap(0) # then fix new root
        else:
            self._size-=1
        return item
    
    def remove_el(self,el):
        if self._size == 0:
            raise ValueError(' Priority queue is empty. ')                   
        if self._size > 1:
            indAct=self._index[el]
            indAct, indDespresSwapp=self._swap(indAct,self._size-1) # put minimum item at the end
            self._size-=1
            self._downheap(indAct) # then fix new root
        else:
            self._size-=1         
        
    def write(self,nomF):
        with open(nomF,"wt") as f1:
            if (self._size == 0): f1.write("|---->BUIT")
            else:
                self.printRec(f1, 0, 0)
    def printRec(self,f1, pos, n):
        if (pos < self._size):
            cad=""
            for i in range(0, n):
                cad+="|--"		
        
        cad+= "|-->" + str(self._data[pos])
        f1.write(cad)
		
        if self.indRight(pos):
            if (self.indRight(pos) < self._size):                
                self.printRec(f1, self.indRight(pos), n + 1)
		
        if self.indLeft(pos) :
            if (self.indLeft(pos) < self._size):
                self.printRec(f1, self.indLeft(pos), n + 1)
   
    def __iter__(self):
        for p in self._data[:self._size]:
            yield p
            
            