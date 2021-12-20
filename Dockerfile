FROM python:3.9-buster

COPY ./Lisp /project/Lisp
COPY ./Prolog /project/Prolog
COPY ./tests /project/tests

WORKDIR /project

RUN apt-get update && apt-get install -y \
    software-properties-common \
    swi-prolog

RUN pip install pyswip

CMD python -m unittest -v

# CMD tail -f /dev/null