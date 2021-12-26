FROM python:3.9-buster

RUN apt-get update && apt-get install -y \
    software-properties-common \
    swi-prolog

RUN pip install pyswip

COPY ./Lisp /project/Lisp
COPY ./Prolog /project/Prolog
COPY ./tests /project/tests

WORKDIR /project

CMD python -m unittest

# CMD tail -f /dev/null