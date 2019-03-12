%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Trabalho Parte 1

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
:- dynamic consulta/4.
:- dynamic instituicao/3.

%--------------------------------- Base de conhecimento - - - - - - - - - -  -  -  -  -   -
% - Utentes

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


% - servicos

servico(1,cardiologia,hospital,braga).
servico(2,enfermaria,hospital,braga).
servico(3,psiquiatria,hospital,guimaraes).
servico(4,podologia,centrodesaude,barcelos).
servico(5,geneacologia,hospital,guimaraes).
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

% - Consultas

consulta(01-02-18,12345,1,50).
consulta(20-03-19,69,5,69).
consulta(12-02-18,12345,1,50).
consulta(22-05-18,12345,7,50).
consulta(25-06-18,12345,15,50).
consulta(25-06-18,23245,15,50).
consulta(02-02-18,11,15,50).
consulta(02-02-18,8,12,50).
consulta(02-02-18,31323,5,50).


%--------------------------------- Predicados auxiliares - - - - - - - - - -  -  -  -  -   -


%-------- (i) - Extensão do Predicado de inserção  --------------


insere(X) :- assert(X).
%insere(X) :- retract(X),!,fail.


%-------- (ii) - Extensão do Predicado de Remoção  --------------


remove(X) :- retract(X).
%remove(X) :- assert(X),!,fail.


%-------- (iii) - Extensão do Predicado de encontrar todas as soluções  --------------


solucoes(T,Q,S) :- findall(T,Q,S).


%-------- (iv) - Extensão do Predicado de remover repetidos  --------------


remOne(X,[],[]).
remOne(X,[X|XS],XS).
remOne(X,[Q|XS],[Y|YS]) :- remOne(X,XS,YS).

%remReps(X,[],[]).
%remReps(X,[H|T],[Y|YS]) :- remOne(X,[H|T],R),remReps(X,T,R).

remReps([],[]).
remReps([H|T],[Y|YS]) :- remOne(H,T,R),remReps(R,YS).


%-------- (v) - Extensão do Predicado de somar todos os elementos de uma lista --------------


sum([],0).
sum([H|T],R) :- sum(T,Y),R is Y+H.


%-------- (vi) - Extensão do Predicado de encontrar o comprimento de uma lista --------------


size([],0).
size([H|T],R) :- size(T,Y), R is 1+Y.


%--------------------------------- Invariantes Estruturais - - - - - - - - - -  -  -  -  -   -


%-------- (i) - Inserção de utente  --------------


+utente(IDU,NOME,CIDADE,IDADE) :- solucoes(IDU,utente(IDU,X,Y,Z),R),size(R,1).


%-------- (ii) - Remoção de utente  --------------

-utente(IDU,NOME,CIDADE,IDADE) :- solucoes(IDU,utente(IDU,X,Y,Z),R),size(R,0).


%-------- (iii) - Inserção de servico  --------------

+servico(IDS,NOME,CIDADE,IDADE) :- solucoes(IDS,utente(IDU,X,Y,Z),R),size(R,1).


%-------- (iv) - Remoção de servico  --------------


-servico(IDS,NOME,CIDADE,IDADE) :- solucoes(IDS,utente(IDU,X,Y,Z),R),size(R,0).


%-------- (v) - Inserção de consulta  --------------


+consulta(IDC,NOME,CIDADE,IDADE) :- solucoes(IDC,utente(IDU,X,Y,Z),R),size(R,1).


%-------- (vi) - Remoção de consulta  --------------


-consulta(IDC,NOME,CIDADE,IDADE) :- solucoes(IDC,utente(IDU,X,Y,Z),R),size(R,0).



%--------------------------------- Controlo de integridade da base de conhecimento - - - - - - - - - -  -  -  -  -   -


teste([]).
teste([H|T]) :- H,teste(T).


evolucao(T) :- solucoes(T,+T::I,R),insere(T).


involucao(T) :- solucoes(T,-T::I,R),remove(T).


%--------------------------------- Funcionalidades - - - - - - - - - -  -  -  -  -   -


%-------- 1 - Extensão do Predicado que permite registar utentes, serviços e consultas --------------




%-------- 2 - Extensão do Predicado que permite eliminar utentes, serviços e consultas --------------





%-------- 3 - Extensão do Predicado que permite identificar todas as Instituições prestadoras de servico --------------

%instituicao : idInst, Nome, Cidade -> {V,F}

instituicao(1,hospital,braga).
instituicao(2,hospital,guimaraes).
instituicao(3,centrodesaude,barcelos).
instituicao(4,clinica,barcelos).
instituicao(5,clinicaprivada,porto).
instituicao(6,hospital,porto).
instituicao(7,hospitalprivado,braga).
instituicao(8,centrodesaude,braga).
instituicao(9,centrodesaude,guimaraes).


%-------- 4 - Extensão do Predicado que permite identificar utentes por id e nome,servico por id e consulta por id de servico e id de utente--------------


idUtente(ID,R) :- solucoes((ID,N,I,C), utente(ID,N,I,C), R),remReps(R,RES).

nomeUtente(NOME,R) :- solucoes((ID,NOME,I,C),utente(ID,NOME,I,C),R),remReps(R,RES).

idServ(ID,R) :- solucoes((ID,SERV,INS,LOC),servico(ID,SERV,INS,LOC)),remReps(R,RES).

servConsulta(ID,R) :- solucoes((DAT,UT,ID,C), consulta(DAT,UT,ID,C), R),remReps(R,RES).

utenteConsulta(ID,R) :- solucoes((DAT,ID,SER,C),consulta(DAT,ID,SER,C),R),remReps(R,RES). 


%-------- 5 - Extensão do Predicado que permite Identificar serviços prestados por instituição/cidade/datas/custo --------------


instServico(INSTITUICAO,R) :- solucoes((ID,DESC,INSTITUICAO,C),servico(ID,DESC,INSTITUICAO,C),R),remReps(R,RES).

cidadeServico(CIDADE,R) :- solucoes((ID,DESC,INSTITUICAO,CIDADE),servico(ID,DESC,INSTITUICAO,CIDADE),R),remReps(R,RES).

dataServico(DAT,R) :- solucoes((ID,DESC,INSTITUICAO,CIDADE),(consulta(DAT,UT,ID,C),servico(ID,DESC,INSTITUICAO,CIDADE)),R),remReps(R,RES).

custoServico(CUSTO,R) :- solucoes((ID,DESC,INSTITUICAO,CIDADE),(consulta(DAT,UT,ID,CUSTO),servico(ID,DESC,INSTITUICAO,CIDADE)),R),remReps(R,RES).


%-------- 6 - Extensão do Predicado que permite identificar os utentes de um serviço/instituição --------------


utenteServico(ID,R) :- solucoes((IDU, NOME, IDADE, CIDADE),(utente(IDU,NOME,IDADE,CIDADE),servico(ID,DESC,INSTITUICAO,CIDADEU),consulta(DAT,IDU,ID,C)),R),remReps(R,RES).

utenteInstituicao(INSTITUICAO,R) :- solucoes((IDU, NOME, IDADE, CIDADE),(utente(IDU,NOME,IDADE,CIDADE),servico(ID,DESC,INSTITUICAO,CIDADEU),consulta(DAT,IDU,ID,C)),R),remReps(R,RES).


%-------- 7 - Extensão do Predicado que permite identificar serviços realizados por utente/instituição/cidade --------------


servicoUtente(IDU,R) :- solucoes((ID,DESC,INST,CID),(utente(IDUse,NOME,IDADE,CIDADE),servico(ID,DESC,INST,CID),consulta(DAT,IDU,ID,C)),R),remReps(R,RES).

servicoInstituicao(INSTITUICAO,R) :- solucoes((ID,DESC,INSTITUICAO,CID),(utente(IDU,NOME,IDADE,CIDADE),servico(ID,DESC,INSTITUICAO,CID),consulta(DAT,IDU,ID,C)),R),remReps(R,RES).

servicoCidade(CIDADE,R) :- solucoes((ID,DESC,INST,CIDADE),(utente(IDU,NOME,IDADE,CID),servico(ID,DESC,INST,CIDADE),consulta(DAT,IDU,ID,C)),R),remReps(R,RES).


%-------- 8 - Extensão do Predicado que permite calcular o custo total dos cuidados de saúde por utente/serviço/instituição/data --------------


custoUtente(IDU,R) :- solucoes(Custo,(utente(IDU,NOME,IDADE,CID),servico(ID,DESC,INST,CIDADE),consulta(DAT,IDU,ID,Custo)),R1),sum(R1,R).

custoServico(IDS,R) :- solucoes(Custo,(utente(IDU,NOME,IDADE,CID),servico(ID,DESC,INST,CIDADE),consulta(DAT,IDU,ID,Custo)),R1),sum(R1,R).

custoInstituicao(INST,R) :- solucoes(Custo,(utente(IDU,NOME,IDADE,CID),servico(ID,DESC,INST,CIDADE),consulta(DAT,IDU,ID,Custo)),R1),sum(R1,R).

custoData(DAT,R) :- solucoes(Custo,(utente(IDU,NOME,IDADE,CID),servico(ID,DESC,INST,CIDADE),consulta(DAT,IDU,ID,Custo)),R1),sum(R1,R).

