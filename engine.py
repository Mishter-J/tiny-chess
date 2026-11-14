import json,os,sys,urllib.request as u,random
V=[0,100,320,330,500,900,0]
T=["K"*32,"KKKKUUUUMMOQLLMPKKKOLJIKLMMGKKKK","ACEECGKKEKMNELNOEKNOELMNCGKLACEE","GIIIIKKKIKLMILLMIKMMIMMMILKKGIII","KKKKLMMMJKKKJKKKJKKKJKKKJKKKKKKL","GIIJIKKKIKLLJKLLKKLLIKLLIKKKGIIJ","ECCAECCAECCAECCAGEECIGGGOOKKOQMK"]
def D(s):
 t=[0]*128
 for r in range(8):
  for f in range(8):t[(7-r)*16+f]=(ord(s[r*4+min(f,7-f)])-65)*5-50
 return t
W=[D(s)for s in T]
X=[[W[i][j^112]for j in range(128)]for i in range(7)]
Y=D("ACEGEGIKEIOQEIQSEIQSEIOQEEKKAEEE")
R=random.Random(7)
Z=[R.getrandbits(64)for _ in range(1700)]
CK=[15]*128
for i,v in((0,13),(7,14),(4,12),(112,7),(119,11),(116,3)):CK[i]=v
JN=(31,33,14,18,-14,-18,-31,-33);KG=(1,-1,16,-16,15,17,-15,-17);DG=(15,17,-15,-17);OG=(1,-1,16,-16)
MATE=100000;NB=120000
bd=[0]*128;sd=1;sc=0;ph=0;cs=0;ep=-1;hm=0;zh=0;kw=4;kb=116;bw=0;bb=0
un=[];tt={};kl=[[0,0]for _ in range(128)];hi=[0]*32768;nd=0;ab=False;hh=[]
def atk(q,w):
 b=bd;a=1 if w>0 else -1
 for o in(-15,-17)if w>0 else(15,17):
  s=q+o
  if not s&136 and b[s]==a:return 1
 for o in JN:
  s=q+o
  if not s&136 and b[s]==2*a:return 1
 for o in KG:
  s=q+o
  if not s&136 and b[s]==6*a:return 1
 for o in DG:
  s=q
  while 1:
   s+=o
   if s&136:break
   x=b[s]
   if x:
    if x==3*a or x==5*a:return 1
    break
 for o in OG:
  s=q
  while 1:
   s+=o
   if s&136:break
   x=b[s]
   if x:
    if x==4*a or x==5*a:return 1
    break
 return 0
def gen(cap):
 m=[];b=bd;s=sd
 for q in range(128):
  if q&136:continue
  p=b[q]
  if not p or(p>0)!=(s>0):continue
  a=abs(p);d=16*s
  if a==1:
   t=q+d
   if not b[t]:
    if(t>>4)==(7 if s>0 else 0):
     m.append(q|(t<<7)|(5<<14));m.append(q|(t<<7)|(2<<14))
    elif not cap:
     m.append(q|(t<<7))
     if(q>>4)==(1 if s>0 else 6)and not b[t+d]:m.append(q|((t+d)<<7))
   for t in(q+d-1,q+d+1):
    if t&136:continue
    x=b[t]
    if x and(x>0)!=(s>0):
     if(t>>4)==(7 if s>0 else 0):m.append(q|(t<<7)|(5<<14));m.append(q|(t<<7)|(2<<14))
     else:m.append(q|(t<<7))
    elif t==ep:m.append(q|(t<<7))
  elif a==2 or a==6:
   for o in JN if a==2 else KG:
    t=q+o
    if t&136:continue
    x=b[t]
    if x:
     if(x>0)!=(s>0):m.append(q|(t<<7))
    elif not cap:m.append(q|(t<<7))
   if a==6 and not cap:
    if s>0 and q==4:
     if cs&1 and not b[5]and not b[6]and b[7]==4 and not atk(4,-1)and not atk(5,-1)and not atk(6,-1):m.append(4|(6<<7))
     if cs&2 and not b[3]and not b[2]and not b[1]and b[0]==4 and not atk(4,-1)and not atk(3,-1)and not atk(2,-1):m.append(4|(2<<7))
    elif s<0 and q==116:
     if cs&4 and not b[117]and not b[118]and b[119]==-4 and not atk(116,1)and not atk(117,1)and not atk(118,1):m.append(116|(118<<7))
     if cs&8 and not b[115]and not b[114]and not b[113]and b[112]==-4 and not atk(116,1)and not atk(115,1)and not atk(114,1):m.append(116|(114<<7))
  else:
   for o in DG if a==3 else OG if a==4 else KG:
    t=q
    while 1:
     t+=o
     if t&136:break
     x=b[t]
     if x:
      if(x>0)!=(s>0):m.append(q|(t<<7))
      break
     elif not cap:m.append(q|(t<<7))
 return m
def make(m):
 global sd,sc,ph,cs,ep,hm,zh,kw,kb,bw,bb
 f=m&127;t=m>>7&127;pr=m>>14&7;p=bd[f];a=abs(p);w=1 if p>0 else -1
 q=bd[t];ec=a==1 and not q and(f&7)!=(t&7);c2=t-16*w if ec else t
 cap=bd[c2] if ec else q
 un.append((m,c2,cap,cs,ep,hm,zh,sc,ph,kw,kb,bw,bb))
 sc-=w*(V[a]+(W[a][f]if w>0 else X[a][f]))
 if cap:
  ca=abs(cap);sc+=w*(V[ca]+(W[ca][c2]if cap>0 else X[ca][c2]))
  if ca!=1:ph-=V[ca]
  if ca==3:
   if cap>0:bw-=1
   else:bb-=1
 na=pr if pr else a
 sc+=w*(V[na]+(W[na][t]if w>0 else X[na][t]))
 if pr:
  ph+=V[pr]
  if pr==3:
   if w>0:bw+=1
   else:bb+=1
 zh^=Z[(p+6)*128+f]^Z[(na*w+6)*128+t]
 if cap:zh^=Z[(cap+6)*128+c2]
 bd[t]=na*w;bd[f]=0
 if ec:bd[c2]=0
 if a==6:
  if abs(t-f)==2:
   rf=f+3 if t>f else f-4;rt=f+1 if t>f else f-1;r=bd[rf];bd[rt]=r;bd[rf]=0
   sc-=w*(V[4]+(W[4][rf]if w>0 else X[4][rf]));sc+=w*(V[4]+(W[4][rt]if w>0 else X[4][rt]))
   zh^=Z[(r+6)*128+rf]^Z[(r+6)*128+rt]
  if w>0:kw=t
  else:kb=t
 nc=cs&CK[f]&CK[t]
 zh^=Z[1664+cs]^Z[1664+nc];cs=nc
 if ep>=0:zh^=Z[1680+(ep&7)]
 ep=(f+t)//2 if a==1 and abs(t-f)==32 else -1
 if ep>=0:zh^=Z[1680+(ep&7)]
 hm=0 if a==1 or cap else hm+1
 sd=-sd;zh^=Z[1688]
 if atk(kw if w>0 else kb,sd):unmake();return 0
 return 1
def unmake():
 global sd,sc,ph,cs,ep,hm,zh,kw,kb,bw,bb
 m,c2,cap,cs,ep,hm,zh,sc,ph,kw,kb,bw,bb=un.pop()
 f=m&127;t=m>>7&127;pr=m>>14&7;p=bd[t];w=1 if p>0 else -1
 bd[f]=w if pr else p;bd[t]=0
 if cap:bd[c2]=cap
 if abs(p)==6 and abs(t-f)==2:
  if t>f:bd[f+3]=bd[f+1];bd[f+1]=0
  else:bd[f-4]=bd[f-1];bd[f-1]=0
 sd=-sd
def ev():
 s=sc+((Y[kw]-Y[kb^112])-(W[6][kw]-X[6][kb]))*(6400-ph)//6400+(30 if bw>=2 else 0)-(30 if bb>=2 else 0)
 return s if sd>0 else -s
def ordr(ms,ply,tm):
 def k(m):
  t=(m>>7)&127;v=bd[t];s=0
  if v:s=100000+abs(v)*16-abs(bd[m&127])
  if(m>>14)&7:s+=200000
  if m==tm:s=1000000
  elif not v and m==kl[ply][0]:s=50000
  elif not v and m==kl[ply][1]:s=40000
  elif not v:s=hi[((0 if sd>0 else 1)<<14)|((m&127)<<7)|t]
  return -s
 ms.sort(key=k)
def qs(a,b,ply):
 global nd,ab
 nd+=1
 if nd>NB:ab=True;return 0
 if ab:return 0
 if ply>=120:return ev()
 if atk(kw if sd>0 else kb,-sd):
  ms=gen(False);ordr(ms,ply,0);n=0;best=-MATE-1
  for m in ms:
   if not make(m):continue
   n+=1;hh.append(zh);s=-qs(-b,-a,ply+1);hh.pop();unmake()
   if ab:return 0
   if s>best:best=s
   if s>=b:return b
   if s>a:a=s
  return -MATE+ply if n==0 else best
 s=ev()
 if s>=b:return b
 if s>a:a=s
 ms=gen(True);ordr(ms,ply,0);sp=s
 for m in ms:
  t=(m>>7)&127;v=bd[t];p=(m>>14)&7
  c=V[abs(v)]if v else V[p]if p else 100 if t==ep and abs(bd[m&127])==1 else 0
  if sp+c+200<=a:continue
  if not make(m):continue
  hh.append(zh);s=-qs(-b,-a,ply+1);hh.pop();unmake()
  if ab:return 0
  if s>=b:return b
  if s>a:a=s
 return a
def sr(d,a,b,ply):
 global nd,ab,hm,zh,sd,ep
 if ply>0 and(hm>=100 or hh.count(zh)>1):return 0
 if d<=0 or ply>=120:return qs(a,b,ply)
 nd+=1
 if nd>NB:ab=True;return 0
 if ab:return 0
 e=tt.get(zh);tm=0
 if e:
  tm=e[3]
  if e[0]>=d:
   s=e[2];f=e[1]
   if f==1:return s
   if f==2 and s>=b:return s
   if f==3 and s<=a:return s
 if d>=3 and ph>0 and not atk(kw if sd>0 else kb,-sd):
  s0=sd;e0=ep;z0=zh;h0=hm
  sd=-sd;zh^=Z[1688]
  if ep>=0:zh^=Z[1680+(ep&7)];ep=-1
  hm+=1;hh.append(zh)
  s=-sr(d-3,-b,-b+1,ply+1)
  hh.pop();sd=s0;ep=e0;zh=z0;hm=h0
  if ab:return 0
  if s>=b:return b
 ms=gen(False);ordr(ms,ply,tm)
 best=-MATE-1;bm=0;a0=a;n=0
 for i,m in enumerate(ms):
  t=(m>>7)&127;pr=(m>>14)&7;ic=bd[t]!=0 or pr!=0 or(abs(bd[m&127])==1 and t==ep)
  if not make(m):continue
  n+=1;hh.append(zh)
  if d>=3 and i>3 and not ic:
   s=-sr(d-2,-b,-a,ply+1)
   if not ab and s>a:s=-sr(d-1,-b,-a,ply+1)
  else:s=-sr(d-1,-b,-a,ply+1)
  hh.pop();unmake()
  if ab:return 0
  if s>best:best=s;bm=m
  if s>a:a=s
  if a>=b:
   if not ic:
    if kl[ply][0]!=m:kl[ply][1]=kl[ply][0];kl[ply][0]=m
    hi[((0 if sd>0 else 1)<<14)|((m&127)<<7)|t]+=d*d
   break
 if n==0:return -MATE+ply if atk(kw if sd>0 else kb,-sd) else 0
 tt[zh]=(d,3 if best<=a0 else 2 if best>=b else 1,best,bm)
 return best
def root(ms):
 global nd,ab
 best=ms[0];nd=0
 for d in range(1,64):
  ab=False;nd0=nd
  sc0=[]
  a=-MATE-1
  for m in ms:
   if not make(m):continue
   hh.append(zh)
   s=-sr(d-1,-MATE-1,-a,1)
   hh.pop();unmake()
   if ab:break
   sc0.append((s,m))
   if s>a:a=s
  if ab or not sc0:break
  sc0.sort(key=lambda x:-x[0])
  best=sc0[0][1];ms=[m for s,m in sc0]
  if sc0[0][0]>=MATE-100:break
 return best
def uci(u):
 f=(int(u[1])-1)*16+ord(u[0])-97;t=(int(u[3])-1)*16+ord(u[2])-97
 return f|(t<<7)|(('nbrq'.index(u[4])+2)<<14 if len(u)==5 else 0)
def tou(m):
 f=m&127;t=m>>7&127;pr=m>>14&7
 s="abcdefgh"[f&7]+str((f>>4)+1)+"abcdefgh"[t&7]+str((t>>4)+1)
 return s+("nbrq"[pr-2] if pr else "")
def setup(f):
 global sd,sc,ph,cs,ep,hm,zh,kw,kb,bw,bb
 for i in range(128):bd[i]=0
 sc=0;ph=0;cs=0;ep=-1;hm=0;zh=0;bw=0;bb=0
 r=7
 for row in f.split()[0].split("/"):
  c=0
  for ch in row:
   if ch.isdigit():c+=int(ch)
   else:
    p="PNBRQK".index(ch.upper())+1
    if ch.islower():p=-p
    sq=r*16+c;bd[sq]=p;a=abs(p);w=1 if p>0 else -1
    sc+=w*(V[a]+(W[a][sq]if w>0 else X[a][sq]));zh^=Z[(p+6)*128+sq]
    if a==6:
     if w>0:kw=sq
     else:kb=sq
    elif a!=1:ph+=V[a]
    if a==3:
     if w>0:bw+=1
     else:bb+=1
    c+=1
  r-=1
 p=f.split()
 sd=1 if p[1]=="w" else -1
 if sd<0:zh^=Z[1688]
 for ch in p[2]:
  if ch=="K":cs|=1
  elif ch=="Q":cs|=2
  elif ch=="k":cs|=4
  elif ch=="q":cs|=8
 zh^=Z[1664+cs]
 if p[3]!="-":
  ep=(int(p[3][1])-1)*16+ord(p[3][0])-97;zh^=Z[1680+(ep&7)]
 hm=int(p[4]);un.clear()
def game(f,mvs):
 global hh
 setup(f);hh=[zh]
 for u in mvs:
  if not make(uci(u)):raise ValueError("bad game move "+u)
  hh.append(zh)
def post(pth,body):
 req=u.Request(os.environ["MAKEFUN_ENV_URL"]+pth,data=json.dumps(body).encode(),headers={"content-type":"application/json"})
 try:
  with u.urlopen(req)as r:return json.load(r)
 except Exception as e:sys.exit(f"{pth}: {e}")
if __name__=="__main__":
 ENV=os.environ.get("MAKEFUN_ENV_URL")or sys.exit("MAKEFUN_ENV_URL is not set")
 while True:
  g=post("/game",{})
  if g["status"]=="done":break
  while g["status"]=="your_move":
   game(g["start_fen"],g["moves"])
   ms=[uci(x)for x in g["legal_moves"]]
   g=post("/move",{"game":g["game"],"move":tou(root(ms))})
  print(f"game {g['game']}: {g['outcome']} by {g['reason']}",file=sys.stderr)
 print(f"done: {g.get('wins')} won, {g.get('draws')} drawn, {g.get('losses')} lost",file=sys.stderr)
