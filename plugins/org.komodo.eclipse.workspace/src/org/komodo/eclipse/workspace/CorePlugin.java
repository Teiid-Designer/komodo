package org.komodo.eclipse.workspace;

import java.net.MalformedURLException;
import java.net.URL;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.komodo.core.KEngine;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.RelationalConstants;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Parameter;
import org.komodo.relational.model.Procedure;
import org.komodo.relational.model.ProcedureResultSet;
import org.komodo.relational.model.ResultSetColumn;
import org.komodo.relational.model.StoredProcedure;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.Parameter.Direction;
import org.komodo.relational.model.TabularResultSet;
import org.komodo.relational.model.View;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.relational.vdb.Permission;
import org.komodo.relational.vdb.Translator;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.repository.LocalRepository;
import org.komodo.repository.LocalRepository.LocalRepositoryId;
import org.komodo.shell.Messages;
import org.komodo.shell.Messages.SHELL;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.RepositoryObserver;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;
import org.modeshape.sequencer.teiid.lexicon.CoreLexicon.ModelType;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle
 */
public class CorePlugin extends AbstractUIPlugin implements StringConstants {

	// The plug-in ID
	public static final String PLUGIN_ID = "org.komodo.eclipse.workspace"; //$NON-NLS-1$
	
	private static final String CORE_REPOSITORY_CONFIG = "workspace-core-plugin-repository-in-memory-config.json";

	// The shared instance
	private static CorePlugin plugin;
	
	private static KEngine kEngine;
	
	private static WorkspaceManager wsManager;
	
	/**
	 * The constructor
	 */
	public CorePlugin() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
	 */
	public void start(BundleContext context) throws Exception {
		super.start(context);
		plugin = this;
		
		kEngine = KEngine.getInstance();
		
		try {
			startKEngine();
			
			//loadTestWorkspace();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
	 */
	public void stop(BundleContext context) throws Exception {
		plugin = null;
		super.stop(context);
		
		try {
			kEngine.shutdown();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	/**
	 * Returns the shared instance
	 *
	 * @return the shared instance
	 */
	public static CorePlugin getDefault() {
		return plugin;
	}
	
	public static KEngine getKEngine() {
		return kEngine;
	}
	
	public static WorkspaceManager getWorkspaceManager() {
		return wsManager;
	}
	
	 private void startKEngine() throws KException, InterruptedException {
        final Repository defaultRepo = kEngine.getDefaultRepository();

		// Latch for awaiting the start of the default repository
		final CountDownLatch updateLatch = new CountDownLatch(1);

		// Observer attached to the default repository for listening for the change of state
		RepositoryObserver stateObserver = new RepositoryObserver() {

            @Override
            public void eventOccurred() {
                updateLatch.countDown();
            }
        };
		defaultRepo.addObserver(stateObserver);

        System.out.println(Messages.getString(SHELL.ENGINE_STARTING));
        kEngine.start();
        System.out.println(SPACE + Messages.getString(SHELL.COMPONENT_STARTED)+ NEW_LINE);

        System.out.println(Messages.getString(SHELL.LOCAL_REPOSITORY_STARTING));

        TimerTask progressTask = new TimerTask() {
            @Override
            public void run() {
            	System.out.println(DOT);
            }
         };

         Timer timer = new Timer();
         timer.schedule(progressTask, 0, 1000);

        // Block the thread until the latch has counted down or timeout has been reached
        boolean localRepoWaiting = updateLatch.await(5, TimeUnit.MINUTES);

        // Cancel timer and display repository message
        timer.cancel();

        if (localRepoWaiting)
        	System.out.println(SPACE + Messages.getString(SHELL.COMPONENT_STARTED));
        else
        	System.out.println(SPACE + Messages.getString(SHELL.LOCAL_REPOSITORY_TIMEOUT_ERROR));

        System.out.println(NEW_LINE);
        System.out.println(NEW_LINE);
        
		 wsManager = WorkspaceManager.getInstance(defaultRepo);
		 
		 System.out.println("Workspace Manager Intialized");
	 }
	 
    private void addToRegistry(ImageRegistry registry, final String imagePath) {
        registry.put(imagePath, getImageDescriptor(imagePath));
    }

    @Override
    protected void initializeImageRegistry(ImageRegistry registry) {
        super.initializeImageRegistry(registry);

        /*
         * Initilialise these descriptors since they are required frequently and from app start
         */
        addToRegistry(registry, CoreConstants.Images.KOMODO_ICON);
    }
    

    /**
     * Retrieves the image associated with the specified key from the {@link org.eclipse.jface.resource.ImageRegistry image
     * registry}, creating the image and registering it if it doesn't already exist. A null key will cause the descriptor for the
     * "No image" image to be returned.
     * 
     * @param key The key associated with the image to retrieve. This must be in the form of the path to the image file relative
     *        to this plug-in's folder; may be null.
     * @return The image associated with the specified key.
     * @since 4.0
     */
    public final Image getImage( final String key ) {
        final ImageRegistry registry = getImageRegistry();
        Image img = registry.get(key);
        if (img != null) {
            return img;
        }
        try {
            createImageDescriptor(key);
            img = registry.get(key);
            return img != null ? img : ImageDescriptor.getMissingImageDescriptor().createImage();
        } catch (final SWTException err) {
        	err.printStackTrace();
            return null;
        }
    }

    /**
     * Retrieves the image descriptor associated with the specified key from the {@link org.eclipse.jface.resource.ImageRegistry
     * image registry}, creating the descriptor and registering it if it doesn't already exist. A null key will cause the
     * descriptor for the "No image" image to be returned.
     * 
     * @param key The key associated with the image descriptor to retrieve. This must be in the form of the path to the image file
     *        relative to this plug-in's folder; may be null.
     * @return The image descriptor associated with the specified key.
     * @since 4.0
     */
    public final ImageDescriptor getImageDescriptor( final String key ) {
        final ImageRegistry registry = getImageRegistry();
        final ImageDescriptor descriptor = registry.getDescriptor(key);
        if (descriptor != null) {
            return descriptor;
        }
        return createImageDescriptor(key);
    }
    
    /**
     * @since 4.0
     */
    private ImageDescriptor createImageDescriptor( final String key ) {
        try {
            final URL url = new URL(getBundle().getEntry("/").toString() + key); //$NON-NLS-1$
            final ImageDescriptor descriptor = ImageDescriptor.createFromURL(url);
            final ImageRegistry registry = getImageRegistry();
            registry.put(key, descriptor);
            return descriptor;
        } catch (final MalformedURLException err) {
        	err.printStackTrace();
            return null;
        }
    }
	
    public static UnitOfWork getTransaction(String ID) {

    	try {
			return CorePlugin.getKEngine().getDefaultRepository().createTransaction(ID, false, null);
		} catch (KException e) {
			e.printStackTrace();
		}
    	
    	return null;
    }
    
    public static void loadTestWorkspace() {
		Repository repo = CorePlugin.getKEngine().getDefaultRepository();
		
		try {
			UnitOfWork transaction = repo.createTransaction("GET WORKSPACE", false, null);
			Vdb vdb = wsManager.createVdb(transaction, null, "ProductsVDB", "extPath");
			vdb.setDescription(transaction, "Description for ProductsVDB");
			vdb.setPreview(transaction, true);
			vdb.setVersion(transaction, 19);
			vdb.setConnectionType(transaction, "BY_VERSION");
			
			DataRole role = vdb.addDataRole(transaction, "Full Access Data Role");
			role.setProperty(transaction, VdbLexicon.DataRole.MAPPED_ROLE_NAMES, (Object[])new String[]{"ROLE1", "ROLE2"});
			role.setDescription(transaction, "Full access to all products data and information.");
			
			Permission perm = role.addPermission(transaction, "ProductModel.productData");
			perm.addMask(transaction, "xxxx-mask-name-xxxxx");
			perm.addCondition(transaction, "xxxx-condition-name-xxxxx");
			perm.setAllowCreate(transaction, true);
			perm.setAllowLanguage(transaction, true);
			
			Translator trans = vdb.addTranslator(transaction, "myOracleOverride", "oracle");
			trans.setDescription(transaction, "Oracle override translator. Customized for performance");
			
			Model model = wsManager.createModel(transaction, vdb, "ProductModel");
			model.setDescription(transaction, "Products Oracle source model");
			model.setProperty(transaction, "vdb:description", "Description for ProductModel....");
			Table table = model.addTable(transaction, "productData");
			table.setDescription(transaction, "General product data");
			Column col = table.addColumn(transaction, "ID");
			col.setDatatypeName(transaction, "string");
			col.setLength(transaction, 58);
			col = table.addColumn(transaction, "Value");
			col.setDatatypeName(transaction, "string");
			col.setLength(transaction, 4000);
			col = table.addColumn(transaction, "Reference");
			col.setDatatypeName(transaction, "string");
			col.setLength(transaction, 4000);
			table = model.addTable(transaction, "productInfo");
			col = table.addColumn(transaction, "ID");
			col.setDatatypeName(transaction, "string");
			col.setLength(transaction, 58);
			col = table.addColumn(transaction, "vendor");
			col.setDatatypeName(transaction, "string");
			col.setLength(transaction, 4000);
			col = table.addColumn(transaction, "quantity");
			col.setDatatypeName(transaction, "integer");
			col.setDefaultValue(transaction, "0");
			col = table.addColumn(transaction, "weight");
			col.setDatatypeName(transaction, "integer");
			col.setDefaultValue(transaction, "1100");
			col.setDescription(transaction, "product weight in lbs");
			
			ModelSource mSource = model.addSource(transaction, "ProductsOracle11");
			mSource.setJndiName(transaction, "products-ds04");
			mSource.setTranslatorName(transaction,  "myOracleOverride");
			
			StoredProcedure proc = model.addStoredProcedure(transaction, "getProductData");
			Parameter param = proc.addParameter(transaction, "PRODUCT_ID");
			param.setDirection(transaction, Direction.IN);
			TabularResultSet rs = (TabularResultSet)proc.setResultSet(transaction, TabularResultSet.class);
			
			param = proc.addParameter(transaction, "CATEGORY");
			param.setDirection(transaction, Direction.IN);
			ResultSetColumn rsc = rs.addColumn(transaction, "c1");
			rsc.setDatatypeName(transaction, "integer");
			rsc = rs.addColumn(transaction, "c2");
			rsc.setDatatypeName(transaction, "integer");
			rsc = rs.addColumn(transaction, "c3");
			rsc.setDatatypeName(transaction, "string");
			rsc.setLength(transaction, 255);
			
			vdb = wsManager.createVdb(transaction, null, "BooksVdb", "extPath");
			model = wsManager.createModel(transaction, vdb, "BooksSource");
			table = model.addTable(transaction, "books");
			col = table.addColumn(transaction, "title");
			col.setDatatypeName(transaction, "string");
			col.setLength(transaction, 300);
			col = table.addColumn(transaction, "author");
			col.setDatatypeName(transaction, "string");
			col.setLength(transaction, 300);
			col = table.addColumn(transaction, "publisherID");
			col.setDatatypeName(transaction, "integer");
			table = model.addTable(transaction, "publishers");
			col = table.addColumn(transaction, "publisherID");
			col.setDatatypeName(transaction, "integer");
			col = table.addColumn(transaction, "name");
			col.setDatatypeName(transaction, "string");
			col.setLength(transaction, 300);
			col = table.addColumn(transaction, "address");
			col.setDatatypeName(transaction, "string");
			col.setLength(transaction, 300);

			
//			param = proc.addParameter(transaction, "PRODUCT_DATA");
//			param.setDirection(transaction, Direction.RETURN);
			
			model = wsManager.createModel(transaction, vdb, "BooksViews");
			model.setModelType(transaction, Model.Type.VIRTUAL);
			View view = model.addView(transaction, "booksViewInfo");
			view.addColumn(transaction, "column_1");
			view.addColumn(transaction, "column_2");
			view.addColumn(transaction, "column_3");
			view.setQueryExpression(transaction, "SELECT * FROM books WHERE title = 'Gone With The Wind'");
			view = model.addView(transaction, "booksSummaryData");
			view.addColumn(transaction, "column_a");
			view.addColumn(transaction, "column_b");
//			Procedure proc = model.addProcedure(transaction, "getBooksInfo");
//			proc.addParameter(transaction, "isbn");
//			proc.addParameter(transaction, "authorID");
//			proc.addParameter(transaction, "publisherID");

			transaction.commit();
			
			
			System.out.println("  CorePlugin.loadTestWorkspace() ENDED");
			System.out.println("     Result Set # Columns = " + rs.getColumns(null).length);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

    }
    
    public static KomodoObject getWorkspaceRoot() {
		Repository repo = CorePlugin.getKEngine().getDefaultRepository();
		
		try {
			KomodoObject rootNode = repo.komodoWorkspace(null);
			return rootNode;
		} catch (KException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		return null;
    }
    
    public static KomodoObject getLibrary() {
		KomodoObject ko = getWorkspaceRoot();
		
		try {
			for( KomodoObject obj : ko.getChildren(null) ) {
				if( obj.getPrimaryType(null).getName().equalsIgnoreCase(KomodoLexicon.Komodo.LIBRARY) ) {
					return obj;
				}
			}
		} catch (KException e) {
			e.printStackTrace();
		}
			
		return null;
    }
    

    protected static LocalRepository _repo = null;

    protected static LocalRepositoryObserver _repoObserver = null;
    
    protected static class LocalRepositoryObserver implements RepositoryObserver {

        private CountDownLatch latch;

        public LocalRepositoryObserver() {
            resetLatch();
        }

        public void resetLatch() {
            latch = new CountDownLatch(1);
        }

        /**
         * @return the latch
         */
        public CountDownLatch getLatch() {
            return this.latch;
        }

        @Override
        public void eventOccurred() {
            latch.countDown();
        }
    }
	    
}
