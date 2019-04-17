%%---------------------------------------------------------------------------------------------
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3 - EXERCICIO 2
%
%	Tiago Baptista  - A75328
%	Ricardo Pereira - A73577
%	Lucas Pereira	- A68547
%
%----------------------------------------------------------------------------------------------
% - Configurações iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).
:- op(900,xfy,'::').

% - Definicoes iniciais de entidades presentes na base de conhecimento:
%utente: IdUt, Nome, Idade, Cidade -> {V,F}
%servico: IdServ, Descrição, Instituição, Cidade -> {V,F}
%consulta: IdCons, Data, IdUt, IdServ, Custo -> {V,F}

% - Opções para permitirem inserções e remoções na base de conhecimento

:- dynamic utente/4.
:- dynamic servico/4.
:- dynamic consulta/5.
:- dynamic instituicao/3.

%---------- Extensão do meta-predicado demo: Questao, Resposta -> {V, F, D}

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

%---------- Extensão do predicado utente: IdU, Nome, Idade, Morada --------------------------

utente(12345,joaocarlos,20,braga).
utente(67810,martagalo,20,barcelos).
utente(11123,carlamuamba,20,luanda).
utente(14156,pedroavec,20,paris).
utente(17189,joaodostrespes,20,braga).
utente(20212,ricardoultra,20,guimaraes).
utente(23245,joanatripeira,20,porto).
utente(26278,joaodasfrigideiras,20,braga).
utente(28293,margaridaleao,20,braga).
utente(31323,fredericasantos,20,beja).
utente(33345,paulotabuada,20,sintra).
utente(51523,adrianodovitoria20,aveiro).
utente(11,tiagotombado,22,braga).
utente(8,ricardosquirtle,22,braga).
utente(88,lucasblastoise,23,braga).

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

servico(1,cardiologia,hospital,braga).
servico(2,enfermaria,hospital,braga).
servico(3,psiquiatria,hospital,guimaraes).
servico(4,podologia,centrodesaude,barcelos).
servico(5,ginecologia,hospital,guimaraes).
servico(6,podologia,clinica,barcelos).
servico(7,dermatologia,clinicaprivada,porto).
servico(8,enfermaria,centrodesaude,barcelos).
servico(9,oftalmologia,hospital,guimaraes).
servico(10,fisioterapia,hospital,porto).
servico(11,ortopedia,clinicaprivada,barcelos).
servico(12,enfermaria,hospital,porto).
servico(13,enfermaria,centrodesaude,guimaraes).
servico(14,clinicageral,centrodesaude,braga).
servico(15,urgencia,hospitalprivado,braga).

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

consulta(1,01-02-18,12345,1,50).
consulta(2,20-03-19,88,5,70).
consulta(3,12-02-18,12345,1,50).
consulta(4,22-05-18,12345,7,50).
consulta(5,25-06-18,12345,15,50).
consulta(6,25-06-18,23245,15,50).
consulta(7,02-02-18,11,15,50).
consulta(8,02-02-18,8,12,50).
consulta(9,02-02-18,31323,5,50).

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

%---------- Invariante Estrutural: não permite inserção de consultas com utentes ou serviços inexistentes --------

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

%--------------------------------------------------------------------------------------------
%---------- Extensão do Predicado que permite registar utentes, serviços e consultas ------

registaU(ID,NOME,IDADE,CIDADE) :- 
	evolucao(utente(ID,NOME,IDADE,CIDADE)).

registaS(ID,DESC,INST,CIDADE) :- 
	evolucao(servico(ID,DESC,INST,CIDADE)).

registaC(ID,DAT,IDU,IDS,Custo) :- 
	evolucao(consulta(ID,DAT,IDU,IDS,Custo)).

%--------------------------------------------------------------------------------------------
%---------- Extensão do Predicado que permite eliminar utentes, serviços e consultas ------

eliminaU(ID,NOME,IDADE,CIDADE) :- 
	involucao(utente(ID,NOME,IDADE,CIDADE)).

eliminaS(ID,DESC,INST,CIDADE) :- 
	involucao(servico(ID,DESC,INST,CIDADE)).

eliminaC(ID,DAT,IDU,IDS,Custo) :- 
	involucao(consulta(ID,DAT,IDU,IDS,Custo)).

%---------- Extensão do Predicado que permite identificar todas as Instituições prestadoras
%----------   de servico --------------------------------------------------------------------
%----------   instituicao : idInst, Nome, Cidade -> {V,F}

instituicao(1,hospital,braga).
instituicao(2,hospital,guimaraes).
instituicao(3,centrodesaude,barcelos).
instituicao(4,clinica,barcelos).
instituicao(5,clinicaprivada,porto).
instituicao(6,hospital,porto).
instituicao(7,hospitalprivado,braga).
instituicao(8,centrodesaude,braga).
instituicao(9,centrodesaude,guimaraes).


%--------------------------------------------------------------------------------------------
%---------- Conhecimento Incerto --------------(PARTE II)---------------------------------------------

%-------------- Utente ------------------
excecao(utente(ID,nome_deconhecido,IDADE,CIDADE)) :-
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


%-------------- Consulta ------------------

excecao(consulta(IDC,_,IDU,IDS,CUSTO)) :-
	consulta(IDC,data_desconhecida,IDU,IDS,CUSTO).

excecao(consulta(IDC,DATA,IDU, _,CUSTO)) :-
	consulta(IDC,DATA,IDU,servico_desconhecido,CUSTO).

excecao(consulta(IDC,DATA,IDU,IDS,_)) :-
	consulta(IDC,DATA,IDU,IDS,custo_desconhecido).

excecao(consulta(IDC,_,IDU,IDS,_)) :-
	consulta(IDC,data_desconhecida,IDU,IDS,custo_desconhecido).


%-------------- Serviço ------------------

excecao(servico(IDS,_,INST,CIDADE)) :-
	servico(IDS,descricao_desconhecida,INST,CIDADE).

excecao(servico(IDS,DESC,_,CIDADE)) :-
	servico(IDS,DESC,instituicao_desconhecida,CIDADE).

excecao(servico(IDS,DESC,INST,_)) :-
	servico(IDS,DESC,INST,cidade_desconhecida).

excecao(servico(IDS,DESC,_,_)) :-
	servico(IDS,DESC,instituicao_desconhecida,cidade_desconhecida).	

%-------------- Instituição ------------------

excecao(instituicao(IDI,NOME,_)) :-
	instituicao(IDI,NOME,cidade_desconhecida).

%--------------------------------------------------------------------------------------------
%---------- Conhecimento Negativo -----------------------------------------------------------





%--------------------------------------------------------------------------------------------
%---------- Conhecimento Imperfeito -----------------------------------------------------------








