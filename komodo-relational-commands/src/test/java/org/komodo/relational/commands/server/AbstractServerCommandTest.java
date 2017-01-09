package org.komodo.relational.commands.server;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Properties;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.commands.AbstractCommandTest;
import org.komodo.relational.teiid.Teiid;
import org.komodo.spi.repository.Descriptor;
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.TeiidPropertyDefinition;
import org.komodo.spi.runtime.TeiidTranslator;
import org.komodo.spi.runtime.TeiidVdb;

@SuppressWarnings( { "javadoc",
                     "nls" } )
public abstract class AbstractServerCommandTest extends AbstractCommandTest {

    // Mock Server Artifacts
    protected static final TeiidVdb VDB1 = mock(TeiidVdb.class);
    protected static final TeiidVdb VDB2 = mock(TeiidVdb.class);
    protected static final TeiidTranslator TRANSLATOR1 = mock(TeiidTranslator.class);
    protected static final TeiidTranslator TRANSLATOR2 = mock(TeiidTranslator.class);
    protected static final TeiidDataSource DS1 = mock(TeiidDataSource.class);
    protected static final TeiidDataSource DS2 = mock(TeiidDataSource.class);
    protected static final String DS_TYPE1 = "DS_TYPE1";
    protected static final String DS_TYPE2 = "DS_TYPE2";

    // Mock server artifact returns
    static {
        // VDBs
        when(VDB1.getName()).thenReturn("VDB1");
        when(VDB2.getName()).thenReturn("VDB2");
        when(VDB1.getVersion()).thenReturn("1");
        when(VDB2.getVersion()).thenReturn("2");
        when(VDB1.isActive()).thenReturn(true);
        when(VDB2.isActive()).thenReturn(false);
        when(VDB1.getModelNames()).thenReturn(Arrays.asList(new String[]{"Model1","Model2"}));
        when(VDB2.getModelNames()).thenReturn(Arrays.asList(new String[]{"Model1","Model2"}));
        when(VDB1.getProperties()).thenReturn(new Properties());
        when(VDB2.getProperties()).thenReturn(new Properties());

        // Translators
        when(TRANSLATOR1.getName()).thenReturn("TRANSLATOR1");
        when(TRANSLATOR2.getName()).thenReturn("TRANSLATOR2");
        when(TRANSLATOR1.getType()).thenReturn("oracle");
        when(TRANSLATOR2.getType()).thenReturn("salesforce");
        when(TRANSLATOR1.getProperties()).thenReturn(new Properties());
        when(TRANSLATOR2.getProperties()).thenReturn(new Properties());

        // DataSources
        when(DS1.getName()).thenReturn("DS1");
        when(DS2.getName()).thenReturn("DS2");
        when(DS1.getDisplayName()).thenReturn("DS1");
        when(DS2.getDisplayName()).thenReturn("DS2");
        when(DS1.getType()).thenReturn("oracle");
        when(DS2.getType()).thenReturn("file");
        when(DS1.getProperties()).thenReturn(new Properties());
        when(DS2.getProperties()).thenReturn(new Properties());
    }

    /**
     * Inits a mock server for the test
     * @param teiidName the name for the server
     * @param serverConnected 'true' if the server state is 'connected', 'false' if not connected.
     * @param vdbs the server VDBs
     * @param dataSources the server Datasources
     * @param translators the server Translators
     * @param dataSourceTypes the server Datasource Types
     * @throws Exception
     */
    protected void initServer(String teiidName, boolean serverConnected,
                              TeiidVdb[] vdbs, TeiidDataSource[] dataSources,
                              TeiidTranslator[] translators, String[] dataSourceTypes) throws Exception {

        // Set up the Teiid mock instance
        Teiid teiid = mock(Teiid.class);
        when(teiid.getName(getTransaction())).thenReturn(teiidName);
        when(teiid.getRepository()).thenReturn(_repo);
        Descriptor descriptor = mock(Descriptor.class);
        when(descriptor.getName()).thenReturn(KomodoLexicon.Teiid.NODE_TYPE);
        when(teiid.getPrimaryType(getTransaction())).thenReturn(descriptor);
        when(teiid.hasDescriptor(getTransaction(), KomodoLexicon.Teiid.NODE_TYPE)).thenReturn(true);
        _repo.add(getTransaction(), null, teiidName, KomodoLexicon.Teiid.NODE_TYPE);

        // The TeiidInstance
        TeiidInstance teiidInstance = mock(TeiidInstance.class);
        when(teiid.getTeiidInstance(getTransaction())).thenReturn(teiidInstance);
        when(teiidInstance.isConnected()).thenReturn(serverConnected);
        when(teiidInstance.hasVdb("myVdb")).thenReturn(false);
        when(teiidInstance.hasVdb("VDB1")).thenReturn(true);
        when(teiidInstance.hasVdb("VDB2")).thenReturn(true);
        when(teiidInstance.dataSourceExists("myDs")).thenReturn(false);
        when(teiidInstance.dataSourceExists("DS1")).thenReturn(true);
        when(teiidInstance.dataSourceExists("DS2")).thenReturn(true);

        // TeiidPropertyDefinitions
        TeiidPropertyDefinition propDefn1 = mock(TeiidPropertyDefinition.class);
        when(propDefn1.getDisplayName()).thenReturn("Prop1");
        when(propDefn1.getDefaultValue()).thenReturn("Value1");
        TeiidPropertyDefinition propDefn2 = mock(TeiidPropertyDefinition.class);
        when(propDefn2.getDisplayName()).thenReturn("Prop2");
        when(propDefn2.getDefaultValue()).thenReturn("Value2");
        when(teiidInstance.getTemplatePropertyDefns(DS_TYPE1)).thenReturn(Arrays.asList(new TeiidPropertyDefinition[]{propDefn1,propDefn2}));
        when(teiidInstance.getTemplatePropertyDefns(DS_TYPE2)).thenReturn(Arrays.asList(new TeiidPropertyDefinition[]{propDefn1,propDefn2}));

        // The returned objects
        if(vdbs!=null) when(teiidInstance.getVdbs()).thenReturn(Arrays.asList(vdbs));
        if(vdbs!=null) when(teiidInstance.getVdb(VDB1.getName())).thenReturn(VDB1);
        if(vdbs!=null) when(teiidInstance.getVdb(VDB2.getName())).thenReturn(VDB2);
        if(dataSources!=null) when(teiidInstance.getDataSources()).thenReturn(Arrays.asList(dataSources));
        if(dataSources!=null) when(teiidInstance.getDataSource(DS1.getName())).thenReturn(DS1);
        if(dataSources!=null) when(teiidInstance.getDataSource(DS2.getName())).thenReturn(DS2);
        if(dataSourceTypes!=null) when(teiidInstance.getDataSourceTypeNames()).thenReturn(new HashSet< String >(Arrays.asList(dataSourceTypes)));
        if(translators!=null) when(teiidInstance.getTranslators()).thenReturn(Arrays.asList(translators));
        if(translators!=null) when(teiidInstance.getTranslator(TRANSLATOR1.getName())).thenReturn(TRANSLATOR1);
        if(translators!=null) when(teiidInstance.getTranslator(TRANSLATOR2.getName())).thenReturn(TRANSLATOR2);

        // Initing the server may change the commands that are available
        wsStatus.updateAvailableCommands();
    }

}
