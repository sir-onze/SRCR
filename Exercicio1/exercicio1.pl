%%---------------------------------------------------------------------------------------------
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3 - EXERCICIO 1
%
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
%consulta: Data, IdUt, IdServ, Custo -> {V,F}

% - Opções para permitirem inserções e remoções na base de conhecimento

:- dynamic utente/4.
:- dynamic servico/4.
:- dynamic consulta/5.
:- dynamic instituicao/3.

%---------- Predicados auxiliares -----------------------------------------------------------

teste([]).
teste([H|T]) :- H,teste(T).

comprimento([],0).
comprimento([H|T],R) :- comprimento(T,Y), R is 1+Y.

soma([],0).
soma([H|T],R) :- soma(T,Y),R is Y+H.

remOne(X,[],[]).
remOne(X,[X|XS],XS).
remOne(X,[Q|XS],[Y|YS]) :- remOne(X,XS,YS).

remReps([],[]).
remReps([H|T],[Y|YS]) :- remOne(H,T,R), remReps(R,YS).

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
utente(69,lucasblastoise,23,braga).

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

%---------- Extensão do predicado consulta: IdC, Data, IdU, IdS, Custo ----------------------

consulta(1,01-02-18,12345,1,50).
consulta(2,20-03-19,69,5,69).
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
%---------- Evolução -----------------------------------------------------------------------

%---------- 2-Extensão do Predicado que permite eliminar utentes, serviços e consultas ------
%---------- Involução -----------------------------------------------------------------------

%---------- 3-Extensão do Predicado que permite identificar todas as Instituições prestadoras
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


%-------- 4-Extensão do Predicado que permite identificar utentes por id e nome,servico por
%--------   id e consulta por id de servico e id de utente --------------------------------

% Identificar utente por id
idUtente(Id, S) :- solucoes((Id, N, I, C), utente(Id, N, I, C), S).

% Identificar utente por nome
nomeUtente(Nome, S) :- solucoes((Id, Nome, I, C), utente(Id, Nome, I, C), S).

% Identificar serviço por id
idServ(Id, S) :- solucoes((Id, D, I, C), servico(Id, D, I, C),S).

% Identificar consultas por serviço
servConsulta(Id, S) :- solucoes((I, D, Id, SE, C), consulta(I, D, Id, SE, C), S).

% Identificar consultas de um utente
utenteConsulta(Id, S) :- solucoes((I, D, Id, SE, C), consulta(I, D, Id, SE, C), S).

%-------- 5 - Extensão do Predicado que permite Identificar serviços prestados por instituição, cidade, datas, custo --------------------------------------------------------------------------

% Identificar serviços efetuados por uma instituição
instServico(I, S) :- solucoes((Id, D, I, C), servico(Id, D, I, C), S), remReps(S, R).

% Identificar serviços por cidade
cidadeServico(CIDADE, R) :-
	solucoes((ID,DESC,INSTITUICAO,CIDADE),
	servico(ID,DESC,INSTITUICAO,CIDADE),R),
	remReps(R,RES).

% Identificar serviços por data
dataServico(DAT,R) :-
	solucoes((ID,DESC,INSTITUICAO,CIDADE),(consulta(Id, DAT,UT,ID,C),
	servico(ID,DESC,INSTITUICAO,CIDADE)), R),
	remReps(R,RES).

% Identificar serviços por custo
custoSer(CUSTO,R) :-
	solucoes((ID,DESC,INSTITUICAO,CIDADE),(consulta(Id,DAT,UT,ID,CUSTO),
	servico(ID,DESC,INSTITUICAO,CIDADE)),R),
	remReps(R,RES).

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
	(utente(IDUse,NOME,IDADE,CIDADE),
		servico(ID,DESC,INST,CID),
		consulta(IDC,DAT,IDU,ID,C)),R).

servicoInstituicao(INSTITUICAO,R) :-
	solucoes((ID,DESC,INSTITUICAO,CID),
		(utente(IDU,NOME,IDADE,CIDADE),
			servico(ID,DESC,INSTITUICAO,CID),
			consulta(IDC,DAT,IDU,ID,C)),R),
	remReps(R,RES).

servicoCidade(CIDADE,R) :-
	solucoes((ID,DESC,INST,CIDADE),
		(servico(ID,DESC,INST,CIDADE),
			utente(IDU,NOME,IDADE,CID),
			consulta(IDC,DAT,IDU,ID,C)),R),
	remReps(R,RES).

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
