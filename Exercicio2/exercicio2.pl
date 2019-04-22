%%---------------------------------------------------------------------------------------------
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3 - EXERCICIO 1
%
%
%----------------------------------------------------------------------------------------------
% - Configurações iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).
:- set_prolog_flag(answer_write_options,[max_depth(0)]).
:- op(900,xfy,'::').
:- op(996, xfy, '&&' ).  % operador de conjuncao
:- op(997, xfy, '$$' ).  % operador de disjuncao 
:- op(998, xfx, '=>' ).  % operador de implicacao 
:- op(999, xfx, '<=>' ). % operador de equivalencia

% - Definicoes iniciais de entidades presentes na base de conhecimento:
%utente: IdUt, Nome, Idade, Cidade -> {V,F}
%servico: IdServ, Descrição, Instituição, Cidade -> {V,F}
%consulta: IdCons, Data, IdUt, IdServ, Custo -> {V,F}

% - Opções para permitirem inserções e remoções na base de conhecimento

:- dynamic utente/4.
:- dynamic servico/4.
:- dynamic consulta/5.
:- dynamic instituicao/3.

%---------- Predicados lógicos -----------------------(PARTE II)-------------------------------

equivalencia( X, X, verdadeiro ) :- X \= desconhecido.
equivalencia( desconhecido, Y, desconhecido ).
equivalencia( X, desconhecido, desconhecido ).
equivalencia( verdadeiro, falso, falso ). 
equivalencia( verdadeiro, falso, falso ). 

implicacao( falso, X, verdadeiro ).
implicacao( X, verdadeiro, verdadeiro ).
implicacao( verdadeiro, desconhecido, desconhecido ). 
implicacao( desconhecido, X, desconhecido ) :- X \= verdadeiro.
implicacao( verdadeiro, falso, falso ).

disjuncao( verdadeiro, X, verdadeiro ).
disjuncao( X, verdadeiro, verdadeiro ).
disjuncao( desconhecido, Y, desconhecido ) :- Y \= verdadeiro.
disjuncao( Y, desconhecido, desconhecido ) :- Y \= verdadeiro.
disjuncao( falso, falso, falso ).

conjuncao( verdadeiro, verdadeiro, verdadeiro ).
conjuncao( falso, _, falso ).
conjuncao( _, falso, falso ).
conjuncao( desconhecido, verdadeiro, desconhecido ).
conjuncao( verdadeiro, desconhecido, desconhecido ).

%---------- Extensão do meta-predicado demo implementado com as nossas definições------------
%---------- Questao, Resposta -> {V, F, D} ----------------------------------(PARTE II)------

demo( P <=> X, V ) :- demo( P, V1 ), demo( X, V2 ), equivalencia( V1, V2, V ), !.
demo( P => X, V )  :- demo( P, V1 ), demo( X, V2 ), implicacao( V1, V2, V ), !.
demo( P $$ X, V )  :- demo( P, V1 ), demo( X, V2 ), disjuncao( V1, V2, V ), !.
demo( P && X, V )  :- demo( P, V1 ), demo( X, V2 ), conjuncao( V1, V2, V ), !.


%---------- Extensão do meta-predicado demo: Questao, Resposta -> {V, F, D} ---(PARTE II)------

demo(Predicado,verdadeiro) :- Predicado.
demo(Predicado, falso) :- -Predicado.
demo(Predicado, desconhecido) :-
	nao(Predicado),
	nao(-Predicado).

%---------- Predicados auxiliares -----------------------------------------------------------

teste([]).
teste([H|T]) :- H,teste(T).

comprimento([],0).
comprimento([H|T],R) :- comprimento(T,Y), R is 1+Y.

soma([],0).
soma([H|T],R) :- soma(T,Y),R is Y+H.

nao(Q) :- Q, !, fail.
nao(Q).

pertence(H,[H|T]).
pertence(X,[H|T]) :-
	X \= H,
pertence(X,T).

remReps([],[]).
remReps([H|T], R) :-
	pertence(H,T),
	remReps(T,R).
remReps([H|T],[H|R]) :-
	nao(pertence(H,T)),
remReps(T,R).

solucoes(T,Q,S) :- findall(T, Q, S).

substitui(P,R) :- remove(P),evolucao(R).
substitui(P,R) :- assert(P),!,fail.

remove_nao_inteiros([],[]).
remove_nao_inteiros([H|T],[H|R]) :- 
	integer(H),
	remove_nao_inteiros(T,R).
remove_nao_inteiros([H|T],R) :- 
	\+integer(H),
	remove_nao_inteiros(T,R).

%---------- Extensão do predicado utente: IdU, Nome, Idade, Morada --------------------------

utente(12345,'joaocarlos',20,'braga').
utente(67810,'martagalo',20,'barcelos').
utente(11123,'carlamuamba',20,'luanda').
utente(14156,'pedroavec',20,'paris').
utente(17189,'joaodostrespes',20,'braga').
utente(20212,'ricardoultra',20,'guimaraes').
utente(23245,'joanatripeira',20,'porto').
utente(26278,'joaodasfrigideiras',20,'braga').
utente(28293,'margaridaleao',20,'braga').
utente(31323,'fredericasantos',20,'beja').
utente(33345,'paulotabuada',20,'sintra').
utente(51523,'adrianodovitoria',20,'aveiro').
utente(11,'tiagotombado',22,'braga').
utente(8,'ricardosquirtle',22,'braga').
utente(88,'lucasblastoise',23,'braga').

%---------- Invariante Estrutural: não permite inserção de utentes duplicados ---------------

+utente(IdU,_,_,_) :: (
    solucoes(IdU, utente(IdU,_,_,_), S),
    comprimento(S, N),
    N == 1
).

%---------- Invariante Estrutural: idade tem que estar compreendida entre 1 e 120 -----------

+utente(_, _, Idade, _) :: (
	integer(Idade),
	Idade >= 0, Idade <120
).

%---------- Invariante Estrutural: não permite a remoção ------------------------------------

-utente(IdU,_,_,_) :: (
	solucoes(IdU, utente(IdU,_,_,_), S),
	comprimento(S, N),
	N == 0
).

%---------- Extensão do predicado serviço: IdS, Descrição, Instituição, Cidade --------------

servico(1,'cardiologia','hospital','braga').
servico(2,'enfermaria','hospital','braga').
servico(3,'psiquiatria','hospital','guimaraes').
servico(4,'podologia','centrodesaude','barcelos').
servico(5,'ginecologia','hospital','guimaraes').
servico(6,'podologia','clinica','barcelos').
servico(7,'dermatologia','clinicaprivada','porto').
servico(8,'enfermaria','centrodesaude','barcelos').
servico(9,'oftalmologia','hospital','guimaraes').
servico(10,'fisioterapia','hospital','porto').
servico(11,'ortopedia','clinicaprivada','barcelos').
servico(12,'enfermaria','hospital','porto').
servico(13,'enfermaria','centrodesaude','guimaraes').
servico(14,'clinicageral','centrodesaude','braga').
servico(15,'urgencia','hospitalprivado','braga').

%---------- Invariante Estrutural: não permite inserção de serviços duplicados ---------------

+servico(IdS,_,_,_) :: (
	solucoes(IdS, servico(IdS,_,_,_), S),
	comprimento(S, N),
	N == 1
).

%---------- Invariante Estrutural: remoção de serviço ---------------------------------------

-servico(IdS,_,_,_) :: (
	solucoes(IdS, servico(IdS,_,_,_), S),
	comprimento(S, N),
	N == 0
).

%---------- Invariante Estrutural: não permite remover um serviço com atos associados -----(PARTE II)-----------

-servico(IDS,_,_,_) :: (
	solucoes(IDS,consulta(_,_,_,IDS,_),L),
	comprimento(L,N),
	N == 0
).

%---------- Extensão do predicado consulta: IdC, Data, IdU, IdS, Custo ----------------------

consulta(1,'01-02-18',12345,1,50).
consulta(2,'20-03-19',88,5,70).
consulta(3,'12-02-18',12345,1,50).
consulta(4,'22-05-18',12345,7,50).
consulta(5,'25-06-18',12345,15,50).
consulta(6,'25-06-18',23245,15,50).
consulta(7,'02-02-18',11,15,50).
consulta(8,'02-02-18',8,12,50).
consulta(9,'02-02-18',31323,5,50).

%---------- Invariante Estrutural: não permite inserção de consultas duplicadas -------------

+consulta(IdC,_,_,_,_) :: (
	solucoes(IdC,consulta(IdC,_,_,_,_), S),
	comprimento(S, N),
	N == 1
).

%---------- Invariante Estrutural: remoção de consulta --------------------------------------

-consulta(IdC,_,_,_,_) :: (
	solucoes(IdC,consulta(IdC,_,_,_,_), S),
	comprimento(S, N),
	N == 0
).

%---------- Invariante Estrutural: não permite inserção de consultas com utentes ou serviços inexistentes --------------------------------------

+consulta(_,_,IDU,IDS,_) :: (
	utente(IDU,_,_,_),
	servico(IDS,_,_,_)
).

%---------- Invariante Estrutural: não permite remoção de consultas com utentes associados (PARTE II)-------

-consulta(_,_,IDU,_,_) :: (
	solucoes(IDU, utente(IDU,_,_,_),L),
	comprimento(L,N),
	N == 0
).

%--------------------------------------------------------------------------------------------
% Extensao do predicado que permite a evolucao do conhecimento

insere(Termo) :- assert(Termo).
insere(Termo) :- retract(Termo), !, fail.

evolucao(Termo) :-
	solucoes(Inv, +Termo::Inv, LInv),
	insere(Termo),
	teste(LInv).

%--------------------------------------------------------------------------------------------
% Extensao do predicado que permite a involucao do conhecimento

remove(Termo) :- retract(Termo).
remove(Termo) :- assert(Termo), !, fail.

involucao(Termo) :-
	Termo,
	solucoes(Inv, -Termo::Inv, LInv),
	remove(Termo),
	teste(LInv).

%---------- FUNCIONALIDADES -----------------------------------------------------------------

%---------- 1-Extensão do Predicado que permite registar utentes, serviços e consultas ------

registaU(ID,NOME,IDADE,CIDADE) :- 
	evolucao(utente(ID,NOME,IDADE,CIDADE)).

registaS(ID,DESC,INST,CIDADE) :- 
	evolucao(servico(ID,DESC,INST,CIDADE)).

registaC(ID,DAT,IDU,IDS,Custo) :- 
	evolucao(consulta(ID,DAT,IDU,IDS,Custo)).

%---------- 2-Extensão do Predicado que permite eliminar utentes, serviços e consultas ------

eliminaU(ID,NOME,IDADE,CIDADE) :- 
	involucao(utente(ID,NOME,IDADE,CIDADE)).

eliminaS(ID,DESC,INST,CIDADE) :- 
	involucao(servico(ID,DESC,INST,CIDADE)).

eliminaC(ID,DAT,IDU,IDS,Custo) :- 
	involucao(consulta(ID,DAT,IDU,IDS,Custo)).

%---------- 3-Extensão do Predicado que permite identificar todas as Instituições prestadoras
%----------   de servico --------------------------------------------------------------------
%----------   instituicao : idInst, Nome, Cidade -> {V,F}

instituicao(1,'hospital','braga').
instituicao(2,'hospital','guimaraes').
instituicao(3,'centrodesaude','barcelos').
instituicao(4,'clinica','barcelos').
instituicao(5,'clinicaprivada','porto').
instituicao(6,'hospital','porto').
instituicao(7,'hospitalprivado','braga').
instituicao(8,'centrodesaude','braga').
instituicao(9,'centrodesaude','guimaraes').


%-------- 4-Extensão do Predicado que permite identificar utentes por id e nome,servico por
%--------   id e consulta por id de servico e id de utente --------------------------------

% Identificar utente por id
idUtente(IDU,S) :- 
	solucoes((IDU,NOME,IDADE,CID), 
	utente(IDU,NOME,IDADE,CID),S).

% Identificar utente por nome
nomeUtente(NOME,S) :- 
	solucoes((IDU,NOME,IDADE,CID), 
	utente(IDU,NOME,IDADE,CID),S).

% Identificar serviço por id
idServico(IDS,S) :- 
	solucoes((IDS,DESC,INST,CID), 
	servico(IDS,DESC,INST,CID),S).

% Identificar consultas por serviço
servConsulta(IDS,S) :- 
	solucoes((IDC,DAT,IDU,IDS,CUS), 
	consulta(IDC,DAT,IDU,IDS,CUS),S).

% Identificar consultas de um utente
utenteConsulta(IDU,S) :- 
	solucoes((IDC,DAT,IDU,IDS,CUS), 
	consulta(IDC,DAT,IDU,IDS,CUS),S).

%-------- 5 - Extensão do Predicado que permite Identificar serviços prestados por instituição, cidade, datas, custo --------------------------------------------------------------------------

% Identificar serviços efetuados por uma instituição
instServico(INST, R) :- 
	solucoes((IDS, DESC, INST, CID), 
	servico(IDS, DESC, INST, CID), S), 
	remReps(S, R).

% Identificar serviços por cidade
cidadeServico(CIDADE, R) :-
	solucoes((ID,DESC,INSTITUICAO,CIDADE),
	servico(ID,DESC,INSTITUICAO,CIDADE),RES),
	remReps(RES,R).

% Identificar serviços por data
dataServico(DAT,R) :-
	solucoes((ID,DESC,INSTITUICAO,CIDADE),(consulta(Id, DAT,UT,ID,C),
	servico(ID,DESC,INSTITUICAO,CIDADE)), RES),
	remReps(RES,R).

% Identificar serviços por custo
custoServico(CUSTO,R) :-
	solucoes((ID,DESC,INSTITUICAO,CIDADE),(consulta(Id,DAT,UT,ID,CUSTO),
	servico(ID,DESC,INSTITUICAO,CIDADE)),RES),
	remReps(RES,R).

%-------- 6 - Extensão do Predicado que permite identificar os utentes de um serviço, instituição -------------------------------------------------------------------------------------------

% 
utenteServico(ID,R) :-
	solucoes((IDU, NOME, IDADE, CIDADE),(utente(IDU,NOME,IDADE,CIDADE),
	servico(ID,DESC,INSTITUICAO,CIDADEU),
	consulta(IDC,DAT,IDU,ID,C)),R).

utenteInstituicao(INSTITUICAO,R) :-
	solucoes((IDU, NOME, IDADE, CIDADE),(utente(IDU,NOME,IDADE,CIDADE),
		servico(ID,DESC,INSTITUICAO,CIDADEU),
		consulta(IDC,DAT,IDU,ID,C)),R).

%-------- 7 - Extensão do Predicado que permite identificar serviços realizados por utente/instituição/cidade ------------------------------------------------------------------------------

servicoUtente(IDU,R) :-
	solucoes((ID,DESC,INST,CID),
	(utente(IDU,NOME,IDADE,CIDADE),
		servico(ID,DESC,INST,CID),
		consulta(IDC,DAT,IDU,ID,C)),RES),
	remReps(RES,R).

servicoInstituicao(INSTITUICAO,R) :-
	solucoes((ID,DESC,INSTITUICAO,CID),
		(utente(IDU,NOME,IDADE,CIDADE),
			servico(ID,DESC,INSTITUICAO,CID),
			consulta(IDC,DAT,IDU,ID,C)),RES),
	remReps(RES,R).

servicoCidade(CIDADE,R) :-
	solucoes((ID,DESC,INST,CIDADE),
		(servico(ID,DESC,INST,CIDADE),
			utente(IDU,NOME,IDADE,CID),
			consulta(IDC,DAT,IDU,ID,C)),RES),
	remReps(RES,R).

%-------- 8 - Extensão do Predicado que permite calcular o custo total dos cuidados de saúde por utente, serviço, instituição edata -----------------------------------------------------------

custoUtente(IDU,R) :-
	solucoes(Custo,
		(utente(IDU,NOME,IDADE,CID),
			servico(ID,DESC,INST,CIDADE),
			consulta(IDC,DAT,IDU,ID,Custo)),R1),
	soma(R1,R).

custoServico(IDS,R) :-
	solucoes(Custo,
		(utente(IDU,NOME,IDADE,CID),
			servico(ID,DESC,INST,CIDADE),
			consulta(IDC,DAT,IDU,ID,Custo)),R1),
	soma(R1,R).

custoInstituicao(INST,R) :-
	solucoes(Custo,
		(utente(IDU,NOME,IDADE,CID),
			servico(ID,DESC,INST,CIDADE),
			consulta(IDC,DAT,IDU,ID,Custo)),R1),
	soma(R1,R).

custoData(DAT,R) :-
	solucoes(Custo,
		(utente(IDU,NOME,IDADE,CID),
			servico(ID,DESC,INST,CIDADE),
			consulta(IDC,DAT,IDU,ID,Custo)),R1),
	soma(R1,R).


%-----------------------------------------(PARTE II)-----------------------------------------
%---------- Conhecimento Incerto - Conhecimento desconhecido mas dentro de um conjunto ------
%----------------------------------- indeterminado de hipóteses -----------------------------

%-------------------------------------------Utente ------------------------------------------
excecao(utente(ID,_,IDADE,CIDADE)) :-
	utente(ID,nome_deconhecido,IDADE,CIDADE).

excecao(utente(ID,NOME,_,CIDADE)) :-
	utente(ID,NOME,idade_desconhecida,CIDADE).

excecao(utente(ID,NOME,IDADE,_)) :-
	utente(ID,NOME,IDADE,cidade_desconhecida).

excecao(utente(ID,_,_,_)) :-
	utente(ID,nome_deconhecido,idade_desconhecida,cidade_desconhecida).

excecao(utente(ID,NOME,_,_)) :-
	utente(ID,NOME,idade_desconhecida,cidade_desconhecida).

excecao(utente(ID,_,IDADE,_)) :-
	utente(ID,nome_deconhecido,IDADE,cidade_desconhecida).

excecao(utente(ID,_,_,CIDADE)) :-
	utente(ID,nome_deconhecido,idade_desconhecida,CIDADE).

utente(676869,'marina',15,cidade_desconhecida).

%------------------------------------------- Consulta -----------------------------------------

excecao(consulta(IDC,_,IDU,IDS,CUSTO)) :-
	consulta(IDC,data_desconhecida,IDU,IDS,CUSTO).

excecao(consulta(IDC,DATA,IDU,_,CUSTO)) :-
	consulta(IDC,DATA,IDU,servico_desconhecido,CUSTO).

excecao(consulta(IDC,DATA,IDU,IDS,_)) :-
	consulta(IDC,DATA,IDU,IDS,custo_desconhecido).

excecao(consulta(IDC,_,IDU,IDS,_)) :-
	consulta(IDC,data_desconhecida,IDU,IDS,custo_desconhecido).


%------------------------------------------- Serviço ------------------------------------------

excecao(servico(IDS,_,INST,CIDADE)) :-
	servico(IDS,descricao_desconhecida,INST,CIDADE).

excecao(servico(IDS,DESC,_,CIDADE)) :-
	servico(IDS,DESC,instituicao_desconhecida,CIDADE).

excecao(servico(IDS,DESC,INST,_)) :-
	servico(IDS,DESC,INST,cidade_desconhecida).

excecao(servico(IDS,DESC,_,_)) :-
	servico(IDS,DESC,instituicao_desconhecida,cidade_desconhecida).	

%------------------------------------------- Instituição ---------------------------------------

excecao(instituicao(IDI,NOME,_)) :-
	instituicao(IDI,NOME,cidade_desconhecida).


%-----------------------------------------(PARTE II)-----------------------------------------
%-----------------------------------Conhecimento Negativo -----------------------------------
%--------------------------------------------------------------------------------------------

-utente(ID,NOME,IDADE,CIDADE) :-
	nao(utente(ID,NOME,IDADE,CIDADE)),
	nao(excecao(utente(ID,NOME,IDADE,CIDADE))).

-consulta(IDC,DATA,IDU,IDS,CUSTO) :-
	nao(consulta(IDC,DATA,IDU,IDS,CUSTO)),
	nao(excecao(consulta(IDC,DATA,IDU,IDS,CUSTO))).

-servico(IDS,DESC,INST,CIDADE) :-
	nao(servico(IDS,DESC,INST,CIDADE)),
	nao(excecao(servico(IDS,DESC,INST,CIDADE))).

-instituicao(IDI,NOME,CIDADE) :-
	nao(instituicao(IDI,NOME,CIDADE)),
	nao(excecao(instituicao(IDI,NOME,CIDADE))).


% um utente tem idade desconhecida, mas de certeza diferente de 5
utente(234,'vitinho',idade_desconhecida,'guimaraes').
-utente(234,'vitinho',5,'guimaraes').

% uma consulta tem um custo desconhecido que de certeza não está entre 0 e 10
consulta(55,'08-08-08',11,12,custo_desconhecido).
-consulta(55,'08-08-08',11,12,C) :- 
	C >= 0,
	C =< 10.

% esta linha serve apenas para anular o carater que o editor reconhece como comentario >

% um servico foi realizado numa instituicao desconhecida que de certeza não foi um centro de saude
servico(20,'ortopedia',instituicao_desconhecida,'braga').
-servico(20,'ortopedia','centrodesaude','braga').

% existe uma instituicao cuja cidade é desconhecida e que de certo nao é o porto
instituicao(15,'posto medico',cidade_desconhecida).
-instituicao(15,'posto medico','porto').

%-----------------------------------------(PARTE II)-----------------------------------------
%---------- Conhecimento Interdito REVISAAAAAOOOO - Conhecimento desconhecido e que nunca poderá ser -------
%----------------------------------- conhecido ----------------------------------------------

% utente 4444 tem cidade que ninguém pode conhecer
utente(4444,'marcelino',15,cidade_desconhecida).
%excecao(utente(IDU,NOME,IDADE,CIDADE)) :- utente(IDU,NOME,IDADE,cidade_desconhecida).
nulo(cidade_desconhecida).
+utente(IDU,NOME,IDADE,CIDADE) :: (
	solucoes(
		(IDU,NOME,IDADE,CIDADE),
		(utente(4444,'marcelino',15,CIDADE),nao(nulo(CIDADE))),
		S),
	comprimento(S,N),
	N == 0).

% utente 5551 tem idade que ninguém pode conhecer
utente(5551,'marcela',idade_desconhecida,'evora').
%excecao(utente(IDU,NOME,IDADE,CIDADE)) :- utente(IDU,NOME,idade_desconhecida,CIDADE).
nulo(idade_desconhecida).
+utente(IDU,NOME,IDADE,CIDADE) :: (
	solucoes(
		(IDU,NOME,IDADE,CIDADE),
		(utente(5551,'marcela',IDADE,'evora'),nao(nulo(IDADE))),
		S),
	comprimento(S,N),
	N == 0).

% consulta 5050 tem utente que ninguém pode conhecer
consulta(5050,'11-11-11',utente_desconhecido,1,20).
%excecao(consulta(IDC,DATA,IDU,IDS,CUSTO)) :- consulta(IDC,DATA,utente_desconhecido,IDS,CUSTO).
nulo(utente_desconhecido).
+consulta(IDC,DATA,IDU,IDS,CUSTO) :: (
	solucoes(
		(IDC,DATA,IDU,IDS,CUSTO), 
		(consulta(5050,'11-11-11',IDU,1,20), nao(nulo(IDU))),
		S),
	comprimento(S,N),
	N == 0). 

%-----------------------------------------(PARTE II)-----------------------------------------
%---------- Conhecimento Impreciso - Conhecimento desconhecido mas dentro de um conjunto ----
%----------------------------------- determinado de hipóteses -------------------------------

% O senhor Cesário tem alguma idade e devido a uma falha na altura do seu registo como utente
% não deu para perceber se mora na "Póvoa de Varzim" ou em "Póvoa de Lanhoso" uma vez que 
% apenas escreveu Póvoa.
excecao(utente(14, 'Cesario', 84, 'Povoa de Varzim')).
excecao(utente(14, 'Cesario', 84, 'Povoa de Lanhoso')).

% Na altura do registo de uma consulta ficou-se na dúvida se o serviço à qual está associada era
% o 5 ou o 15.
excecao(consulta(10,'11-05-19',8,5,25)).
excecao(consulta(10,'11-05-19',8,15,25)).

% Na altura do registo de um utente não se apontou a idade do utente mas sabe-se que tem entre
% 25 e 30 anos.
excecao(utente(65,'Miguelinho',D,'Pacos de Ferreira')) :- D >= 25, D =< 30.
% esta linha serve apenas para anular o carater que o editor reconhece como comentario >

%% Na altura do registo de uma consulta não se apontou o custo da mesma mas sabe-se que ficou entre
% 50 e 75 euros.
excecao(consulta(15,'11-05-19',51523,13,C)) :- 
	C >= 50, 
	C =< 75.
% esta linha serve apenas para anular o carater que o editor reconhece como comentario >


%-----------------------------------------(PARTE II)-----------------------------------------
%---------- Transformar conhecimento imperfeito em conhecimento perfeito --------------------
%--------------------------------------------------------------------------------------------


substituir_utente_cidade(IDU,CIDADE) :- solucoes_utente(IDU,utente(IDU,N,I,C)) ,
										substitui(utente(IDU,N,I,C),utente(IDU,N,I,CIDADE)).

substituir_utente_idade(IDU,IDADE) :- solucoes_utente(IDU,utente(IDU,N,I,M)),
									  substitui(utente(IDU,N,I,M),utente(IDU,N,IDADE,M)).

substituir_utente_nome(IDU,NOME) :- solucoes_utente(IDU,utente(IDU,N,I,M)),
									substitui(utente(IDU,N,I,M),utente(IDU,NOME,I,M)).

substituir_consulta_servico(IDC,SERVICO) :- solucoes_consulta(IDC,consulta(IDC,D,U,S,C)),
										    substitui(consulta(IDC,D,U,S,C),consulta(IDC,D,U,SERVICO,C)).

substituir_consulta_custo(IDC,CUSTO) :- solucoes_consulta(IDC,consulta(IDC,D,U,S,C)),
										substitui(consulta(IDC,D,U,S,C),consulta(IDC,D,U,S,CUSTO)).

substituir_consulta_data(IDC,DATA) :- solucoes_consulta(IDC,consulta(IDC,D,U,S,C)),
									  substitui(consulta(IDC,D,U,S,C),consulta(IDC,DATA,U,S,C)).

substituir_servico_instituicao(IDS,INSTITUICAO) :- solucoes_servico(IDS,servico(IDS,D,I,C)),
												   substitui(servico(IDS,D,I,C),servico(IDS,D,INSTITUICAO,C)).

substituir_servico_cidade(IDS,CIDADE) :- solucoes_servico(IDS,servico(IDS,D,I,C)),
										 substitui(servico(IDS,D,I,C),servico(IDS,D,INSTITUICAO,CIDADE)).


%-----------------------------------------(PARTE II)-----------------------------------------
%---------------------------------- Funcionalidades Extra -----------------------------------
%--------------------------------------------------------------------------------------------

%-------------------- Predicado que permite identificar um utente dado um id ----------------

% NOTA: o resultado é guardado como elemento de uma lista para que depois possa chamar o 
% predicado com utente,de outra maneira o prolog ficava à espera de uma lista.

solucoes_utente(IDU,R) :- solucoes(utente(IDU,N,I,C),utente(IDU,N,I,C),[R|_]).

%-------------------- Predicado que permite identificar uma consulta dado um id ----------------

solucoes_consulta(IDC,R) :- solucoes(consulta(IDC,D,U,S,C),consulta(IDC,D,U,S,C),[R|_]).

%-------------------- Predicado que permite identificar um servico dado um id ----------------

solucoes_servico(IDS,R) :- solucoes(servico(IDS,D,I,C),servico(IDS,D,I,C),[R|_]).

%-------------------- Predicado que permite identificar o numero de consultas efetuadas por um
% dado servico -------------------------------------------------------------------------------

total_consultas(IDS,R) :- solucoes((IDC,D,U,IDS,C),(servico(IDS,DS,I,CS),consulta(IDC,D,U,IDS,C)),S),comprimento(S,R).

%-------------------- Predicado que permite identificar todos os utentes por cidade ----------

utentes_cidade(CIDADE,R) :- solucoes((IDU,N,I,CIDADE),utente(IDU,N,I,CIDADE),R).

%-------------------- Predicado que permite calcular a média de uma lista -------------------

media(X,Y,R) :- R is (X/Y).


%-------------------- Predicado que permite calcular o numero de utentes que realizaram
% consultas no sistema ----------------------------------------------------------------------

numero_utentes(R) :- solucoes(Custo,
					(utente(IDU,NOME,IDADE,CID),
					servico(ID,DESC,INST,CIDADE),
					consulta(IDC,DAT,IDU,ID,Custo)),R1),
					comprimento(R1,R).

%-------------------- Predicado que permite calcular o total gasto por todos os utentes no
% sistema -----------------------------------------------------------------------------------

custo_utentes(R) :- solucoes(Custo,
					(utente(IDU,NOME,IDADE,CID),
					servico(ID,DESC,INST,CIDADE),
					consulta(IDC,DAT,IDU,ID,Custo)),R1),
					remove_nao_inteiros(R1,R2),
					soma(R2,R).

%-------------------- Predicado que permite a media de gastos por utente ---------

custo_medio_utente(R) :- 
	numero_utentes(Y),
	custo_utentes(X),
	R is X/Y.

