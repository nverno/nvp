sml      ?= sml
ml-build ?= ml-build

CLIENT   = client-image
SERVER   = server-image
SUFFIX   = x86-linux
TARGETS  = ${CLIENT}.${SUFFIX} ${SERVER}.${SUFFIX}

all: ${TARGETS}

${CLIENT}.${SUFFIX}: client.cm client.sml
	${ml-build} client.cm Client.main ${CLIENT}

${SERVER}.${SUFFIX}: server.cm server.sml
	${ml-build} server.cm Server.main ${SERVER}

clean:
	$(RM) -rf .cm core *~ *.o *.out *.exe *image*
