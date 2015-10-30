/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest;

import java.io.File;
import java.io.InputStream;
import java.util.List;
import java.util.concurrent.TimeUnit;
import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.ServerErrorException;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.ResponseBuilder;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.core.UriInfo;
import org.komodo.core.KEngine;
import org.komodo.importer.ImportMessages;
import org.komodo.importer.ImportOptions;
import org.komodo.importer.ImportOptions.OptionKeys;
import org.komodo.importer.vdb.VdbImporter;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.repository.SynchronousCallback;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.relational.RelationalMessages;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.schema.json.TeiidXsdReader;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.Id;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;

/**
 * A Komodo REST service for obtaining VDB information from the workspace.
 */
@Path( V1Constants.SERVICE_SEGMENT )
@Api( tags = {V1Constants.SERVICE_SEGMENT} )
public final class KomodoUtilService extends KomodoService {

    private static final String REPO_WKSP_LABEL = "Repository Workspace"; //$NON-NLS-1$

    private static final String REPO_CONFIG_LABEL = "Repository Configuration"; //$NON-NLS-1$

    private static final String REPO_VDB_TOTAL = "Repository Vdb Total"; //$NON-NLS-1$

    /**
     * The sample vdbs provided by this service
     */
    @SuppressWarnings( "nls" )
    public static final String[] SAMPLES = {
        "parts_dynamic-vdb.xml", "portfolio-vdb.xml",
        "teiid-vdb-all-elements.xml", "tweet-example-vdb.xml"
      };

    private WorkspaceManager wsMgr;

    /**
     * @param engine
     *        the Komodo Engine (cannot be <code>null</code> and must be started)
     * @throws ServerErrorException
     *         if there is a problem obtaining the {@link WorkspaceManager workspace manager}
     */
    public KomodoUtilService(final KEngine engine) throws ServerErrorException {
        super(engine);

        try {
            this.wsMgr = WorkspaceManager.getInstance(this.repo);
        } catch (final Exception e) {
            throw new ServerErrorException(Messages.getString(Messages.Error.KOMODO_ENGINE_WORKSPACE_MGR_ERROR), Status.INTERNAL_SERVER_ERROR);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return about information of this service
     * @throws KomodoRestException if error occurs
     */
    @GET
    @Path(V1Constants.ABOUT)
    @ApiOperation( value = "Display status of this rest service", response = String.class )
    public Response about(final @Context HttpHeaders headers,
                                          final @Context UriInfo uriInfo) throws KomodoRestException {
        KomodoStatusObject repoStatus = new KomodoStatusObject();

        Id id = this.repo.getId();
        repoStatus.addAttribute(REPO_WKSP_LABEL, id.getWorkspaceName());
        repoStatus.addAttribute(REPO_CONFIG_LABEL, id.getConfiguration().toString());

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;
        try {
            // find VDBs
            uow = createTransaction("getVdbs", true); //$NON-NLS-1$
            Vdb[] vdbs = this.wsMgr.findVdbs(uow);

            repoStatus.addAttribute(REPO_VDB_TOTAL, Integer.toString(vdbs.length));

        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            String errorMsg = e.getLocalizedMessage() != null ? e.getLocalizedMessage() : e.getClass().getSimpleName();
            errorMsg = RelationalMessages.getString(RelationalMessages.Error.VDB_SERVICE_GET_VDBS_ERROR, errorMsg);
            repoStatus.addAttribute(REPO_VDB_TOTAL, errorMsg);
        }

        // create response
        try {
            return commit(uow, mediaTypes, repoStatus);
        } catch (Exception ex) {
            throw new KomodoRestException(ex);
        }
    }

    private InputStream getVdbSample(String fileName) {
        String sampleFilePath = "sample" + File.separator + fileName; //$NON-NLS-1$
        InputStream fileStream = getClass().getResourceAsStream(sampleFilePath);
        if (fileStream == null)
            LOGGER.error(RelationalMessages.getString(
                                                      RelationalMessages.Error.VDB_SAMPLE_CONTENT_FAILURE, fileName));

        else
            LOGGER.info(RelationalMessages.getString(
                                                     RelationalMessages.Error.VDB_SAMPLE_CONTENT_SUCCESS, fileName));

        return fileStream;
    }

    /**
     * Attempt to import the sample data into the engine
     *
     * @return the response indicating the sample data load has been attempted
     */
    @SuppressWarnings( "nls" )
    @GET
    @Path(V1Constants.SAMPLE_DATA)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Import sample data into VdbBuilder and display the status of the operation",
                             response = KomodoStatusObject.class)
    public Response importSampleData() {

        KomodoStatusObject status = new KomodoStatusObject("Sample Vdb Import");

        for (String sampleName : SAMPLES) {
            InputStream sampleStream = getVdbSample(sampleName);
            if (sampleStream == null) {
                status.addAttribute(sampleName, RelationalMessages.getString(
                                                          RelationalMessages.Error.VDB_SAMPLE_CONTENT_FAILURE, sampleName));
                continue;
            }

            UnitOfWork uow = null;
            try {
                SynchronousCallback callback = new SynchronousCallback();
                uow = repo.createTransaction("Import vdb " + sampleName, false, callback); //$NON-NLS-1$

                ImportOptions importOptions = new ImportOptions();
                importOptions.setOption(OptionKeys.NAME, sampleName);
                ImportMessages importMessages = new ImportMessages();

                KomodoObject workspace = repo.komodoWorkspace(uow);
                VdbImporter importer = new VdbImporter(repo);
                importer.importVdb(uow, sampleStream, workspace, importOptions, importMessages);

                uow.commit();
                if (callback.await(3, TimeUnit.MINUTES))
                    status.addAttribute(sampleName, RelationalMessages.getString(
                                                                                 RelationalMessages.Error.VDB_SAMPLE_IMPORT_SUCCESS, sampleName));
                else
                    status.addAttribute(sampleName, RelationalMessages.getString(
                                                                                 RelationalMessages.Error.VDB_SAMPLE_IMPORT_TIMEOUT, sampleName));

            } catch ( final Exception e ) {
                if ( ( uow != null ) && ( uow.getState() != State.COMMITTED ) ) {
                    uow.rollback();
                }

                status.addAttribute(sampleName, RelationalMessages.getString(
                                                                             RelationalMessages.Error.VDB_SERVICE_LOAD_SAMPLE_ERROR, sampleName, e));
            }
        }

        ResponseBuilder builder = Response.ok( KomodoJsonMarshaller.marshall(status, true), MediaType.APPLICATION_JSON );
        return builder.build();
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return a JSON document representing the schema of the teiid VDB (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem constructing the VDBs JSON document
     */
    @GET
    @Path(V1Constants.SCHEMA_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON } )
    @ApiOperation(value = "Display the schema structure of the teiid vdb",
                            response = String.class)
    public Response getSchema( final @Context HttpHeaders headers,
                             final @Context UriInfo uriInfo ) throws KomodoRestException {

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();

        try {
            TeiidXsdReader reader = new TeiidXsdReader();
            String schema = reader.read();

            ResponseBuilder builder = null;
            if (isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
                builder = Response.ok().entity(schema);
            else
                builder = notAcceptableMediaTypesBuilder();

            return builder.build();

        } catch ( final Exception e ) {
            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            String errorMsg = e.getLocalizedMessage() != null ? e.getLocalizedMessage() : e.getClass().getSimpleName();
            throw new KomodoRestException( RelationalMessages.getString(
                                                                             RelationalMessages.Error.SCHEMA_SERVICE_GET_SCHEMA_ERROR, errorMsg ), e );
        }
    }
}
